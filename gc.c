#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "intern.h"
#include "libscary.h"
#include "schaf.h"
#include "utils.h"

//
// General definitions
//

#ifdef DEBUG
#define xmalloc(size) xcalloc(1, size);
#endif

// Types
enum {
    MiB = 1024 * 1024,
    HEAP_RATIO = 2,
    TABMAX = 1024,
    PTR_ALIGN = 16U,
};

typedef struct {
    size_t size, used;
    size_t tab_free[TABMAX], tab_used[TABMAX];
    size_t *tab_free_larger, *tab_used_larger;
} HeapStat;

typedef struct {
    void (*init)(void);
    void (*fin)(void);
    void *(*malloc)(size_t size);
    void (*add_root)(const Value *r);
    void (*stat)(HeapStat *stat);
} GCFunctions;

static void heap_print_stat(const char *header);
[[noreturn]] static void error_out_of_memory(void);

// Static data
static void *gc_data; // singleton; maybe a heap or some context
static GCFunctions funcs;
static size_t init_size = 1 * MiB;

static const void *stack_base;

static bool stress, print_stat, initialized;

//
// Algorithm: Mark-and-sweep
//

typedef struct MSHeader {
    bool used;
    bool living;
    size_t size;
    alignas(PTR_ALIGN) struct MSHeader *next;
} MSHeader;
enum {
    MS_HEADER_OFFSET = offsetof(MSHeader, next)
};
#define MS_HEADER(p) ((MSHeader *) (p))
#define MS_HEADER_FROM_VAL(v) MS_HEADER((uint8_t *)(v) - MS_HEADER_OFFSET)

typedef struct {
    size_t size;
    uint8_t *body;
} MSHeapSlot;

typedef struct {
    size_t size;
    // 64 is enough large, it can use up the entire 64-bit memory space
    // (= (fold + 0 (map (cut expt 2 <>) (iota 64))) (- (expt 2 64) 1)) ;;=> #t
    MSHeapSlot *slot[64];
    uint8_t *low, *high;
    Value **roots;
    MSHeader *free_list;
    uint8_t *bitmap;
} MSHeap;

#ifdef __clang__
#pragma clang diagnostic ignored "-Wcast-align"
#endif

static inline size_t align(size_t size)
{
    return iceil(size, PTR_ALIGN);
}

static void init_header(MSHeader *h, size_t size, MSHeader *next)
{
    h->used = false;
    h->living = false;
    h->size = size;
    h->next = next;
}

#define HSIZE(size) (MS_HEADER_OFFSET + (size)) // size with header
static MSHeapSlot *ms_heap_slot_new(size_t size)
{
    MSHeapSlot *s = xmalloc(sizeof(MSHeapSlot));
    size = align(size);
    size_t hsize = HSIZE(size);
    s->size = hsize;
    s->body = xmalloc(hsize);
    init_header(MS_HEADER(s->body), size, NULL);
    return s;
}

static void ms_init(void)
{
    MSHeap *heap = xmalloc(sizeof(MSHeap));
    heap->roots = scary_new(sizeof(Value *));
    heap->slot[0] = ms_heap_slot_new(init_size);
    MSHeapSlot *first = heap->slot[0];
    heap->size = 1;
    heap->low = first->body;
    heap->high = heap->low + first->size;
    heap->free_list = MS_HEADER(first->body);
    heap->bitmap = NULL;
    gc_data = heap;
}

static void ms_add_root(const Value *r)
{
    MSHeap *heap = gc_data;
    scary_push((void ***) &heap->roots, (const void *) r);
}

static bool in_heap_range(Value v)
{
    MSHeap *heap = gc_data;
    const uint8_t *p = (uint8_t *) v;
    for (size_t i = 0; i < heap->size; i++) {
        const MSHeapSlot *slot = heap->slot[i];
        if (p > slot->body && p < slot->body + slot->size)
            return true;
    }
    return false;
}

static bool is_valid_pointer(Value v)
{
    return v % PTR_ALIGN == 0 && v > 0; // XXX
}

static bool is_valid_flags(const MSHeader *h)
{
    uint8_t living = *(const uint8_t *) &h->living;
    uint8_t used = *(const uint8_t *) &h->used;
    uint8_t u = living | used;
    return u == true || u == false;
}

static bool is_valid_header(Value v)
{
    UNPOISON(&VALUE_TAG(v), sizeof(ValueTag));         // Suspicious but
    UNPOISON(MS_HEADER_FROM_VAL(v), sizeof(MSHeader)); // need to be read
    if (VALUE_TAG(v) > TAG_LAST)
        return false;
    const MSHeader *h = MS_HEADER_FROM_VAL(v);
    if (!is_valid_flags(h))
        return false;
    size_t size = h->size;
    return size > 0 && size % PTR_ALIGN == 0 &&
        size <= sizeof(Continuation) + MS_HEADER_OFFSET;
}

static bool is_heap_value(Value v)
{
    return is_valid_pointer(v) && in_heap_range(v) && is_valid_header(v);
}

static uintptr_t bitmap_index(const void *p);

static bool is_living(MSHeader *h, bool do_mark)
{
    MSHeap *heap = gc_data;
    bool marked;
    if (heap->bitmap == NULL) {
        marked = h->living;
        if (marked != do_mark)
            h->living = do_mark;
    } else {
        uintptr_t index = bitmap_index(h);
        uint8_t offset = index % 8U;
        index /= 8U;
        uint8_t mask = 1UL << offset;
        marked = heap->bitmap[index] & mask;
        if (do_mark && !marked) // no need to unflag while sweep() because bitmap
            heap->bitmap[index] |= mask; // is immediately freed after that
    }
    return marked;
}

typedef void (*ValueVisitor)(Value v);

static void visit_array(ValueVisitor visit, const void *volatile beg, size_t n)
{
    UNPOISON(beg, n * sizeof(uintptr_t));
    const Value *volatile p = beg;
    for (size_t i = 0; i < n; i++, p++)
        visit(*p);
}

static void visit_jmpbuf(ValueVisitor visit, const jmp_buf *jmp)
{
    visit_array(visit, jmp, idivceil(sizeof(jmp_buf), sizeof(uintptr_t)));
}

static void visit_env_each(UNUSED uint64_t key, uint64_t value, void *p)
{
    ValueVisitor visit = p;
    // key is always a Symbol
    visit(value);
}

static void visit_children(Value v, ValueVisitor visit)
{
    switch (VALUE_TAG(v)) {
    case TAG_PAIR: {
        Pair *p = PAIR(v);
        visit(p->car);
        visit(p->cdr);
        break;
    }
    case TAG_CLOSURE: {
        Closure *p = CLOSURE(v);
        visit(p->env);
        visit(p->params);
        visit(p->body);
        break;
    }
    case TAG_CONTINUATION: {
        Continuation *p = CONTINUATION(v);
        visit(p->retval);
        visit_jmpbuf(visit, &p->state);
        visit_array(visit, p->stack, idivceil(p->stack_len, sizeof(uintptr_t)));
        break;
    }
    case TAG_CFUNC_CLOSURE:
        visit(CFUNC_CLOSURE(v)->data);
        break;
    case TAG_ENV: {
        Env *p = ENV(v);
        if (p->table != NULL)
            table_foreach(p->table, visit_env_each, visit);
        visit(p->parent);
        break;
    }
    case TAG_VECTOR: {
        Value *p = VECTOR(v);
        for (int64_t i = 0, len = scary_length(p); i < len; i++)
            visit(p[i]);
        break;
    }
    case TAG_PROMISE: {
        Promise *p = PROMISE(v);
        visit(p->env);
        visit(p->val);
        break;
    }
    case TAG_STRING:
    case TAG_CFUNC:
    case TAG_SYNTAX:
    case TAG_PORT:
    case TAG_EOF:
    case TAG_ERROR:
        break;
    }
}

static void mark_val(Value v)
{
    if (is_heap_value(v) && // mark with `true`
        !is_living(MS_HEADER_FROM_VAL(v), true))
        visit_children(v, mark_val);
}

static void *allocate_from_chunk(MSHeap *heap, MSHeader *prev, MSHeader *curr, size_t size)
{
    size_t hsize = HSIZE(size);
    MSHeader *next = curr->next;
    if (curr->size > hsize) {
        MSHeader *rest = (MSHeader *)((uint8_t *) curr + hsize);
        init_header(rest, curr->size - hsize, next);
        next = rest;
        curr->size = size;
    }
    if (prev == NULL)
        heap->free_list = next;
    else
        prev->next = next;
    curr->used = true;
    void *p = &curr->next;
    return p;
}

static void add_to_free_list(MSHeap *heap, MSHeader *h)
{
    h->next = heap->free_list; // prepend
    heap->free_list = h;
}

static bool is_user_opened_file(FILE *fp)
{
    return fp != NULL &&
        fp != stdin && fp != stdout && fp != stderr;
}

#ifdef DEBUG
#define cfree(f, p) f(p), p = NULL
#define free(p) cfree(free, p)
#define table_free(p) cfree(table_free, p)
#define scary_free(p) cfree(scary_free, p)
#endif

static void free_val(Value v)
{
    switch (VALUE_TAG(v)) {
    case TAG_CONTINUATION:
        free(CONTINUATION(v)->stack);
        break;
    case TAG_STRING:
        free(STRING(v));
        break;
    case TAG_ENV:
        table_free(ENV(v)->table);
        break;
    case TAG_PORT: {
        Port *p = PORT(v);
        if (is_user_opened_file(p->fp))
            fclose(p->fp);
        if (p->string != (void *) 1U) // avoid mark
            free(p->string);
        break;
    }
    case TAG_VECTOR:
        scary_free(VECTOR(v));
        break;
    case TAG_ERROR: {
        StackFrame **e = ERROR(v);
        for (size_t i = 0, len = scary_length(e); i < len; i++)
            free(e[i]);
        scary_free(e);
        break;
    }
    case TAG_CFUNC:
    case TAG_SYNTAX:
    case TAG_CFUNC_CLOSURE:
    case TAG_CLOSURE:
    case TAG_PAIR:
    case TAG_PROMISE:
    case TAG_EOF:
        break;
    }
}
#ifdef DEBUG
#undef cfree
#undef free
#undef table_free
#undef scary_free
#endif

static bool adjoining_p(MSHeader *free_list, const MSHeader *curr)
{
    if (free_list == NULL)
        return false;
    size_t hsize = HSIZE(free_list->size);
    return (uint8_t *) free_list + hsize == (void *) curr;
}

static void free_chunk(MSHeap *heap, MSHeader *curr)
{
    MSHeader *free_list = heap->free_list;
    Value val = (Value) &curr->next;
    free_val(val);
    if (adjoining_p(free_list, curr)) {
        free_list->size += HSIZE(curr->size);
#ifdef DEBUG
        memset(&curr->next, 0, curr->size);
        memset(curr, 0, sizeof(MSHeader));
#else
        curr->size = 0; // XXX: affects is_valid_header but really needed?
#endif
        return;
    }
    curr->used = false;
    add_to_free_list(heap, curr);
}

static void sweep_slot(MSHeap *heap, MSHeapSlot *slot)
{
    uint8_t *p = slot->body, *endp = p + slot->size;
    size_t offset;
    for (MSHeader *h; p < endp; p += offset) {
        h = MS_HEADER(p);
        offset = HSIZE(h->size);
        if (h->used && !is_living(h, false))
            free_chunk(heap, h);
    }
}

static void sweep(MSHeap *heap)
{
    for (size_t i = 0; i < heap->size; i++)
        sweep_slot(heap, heap->slot[i]);
    free(heap->bitmap);
    heap->bitmap = NULL;
}

static void ms_add_slot(MSHeap *heap)
{
    MSHeapSlot *last = heap->slot[heap->size - 1];
    size_t new_size = last->size * HEAP_RATIO;
    MSHeapSlot *next = heap->slot[heap->size++] = ms_heap_slot_new(new_size);
    uint8_t *beg = next->body, *end = beg + next->size;
    if (heap->low > beg)
        heap->low = beg;
    else if (heap->high < end)
        heap->high = end;
    add_to_free_list(heap, MS_HEADER(beg));
}

static void *ms_allocate(MSHeap *heap, size_t size)
{
    MSHeader *free_list = heap->free_list;
    for (MSHeader *prev = NULL, *curr = free_list; curr != NULL; prev = curr, curr = curr->next) {
        if (curr->size >= size) // First-fit
            return allocate_from_chunk(heap, prev, curr, size);
    }
    return NULL;
}

static void mark_roots(Value **roots)
{
    for (size_t i = 0, len = scary_length(roots); i < len; i++)
        mark_val(*roots[i]);
}

static void mark_jmpbuf(const jmp_buf *jmp)
{
    visit_array(mark_val, jmp, idivceil(sizeof(jmp_buf), sizeof(uintptr_t)));
}

[[gnu::noinline]]
static void mark_stack(void)
{
    GET_SP(sp);
    const uintptr_t *beg = stack_base, *end = sp;
    visit_array(mark_val, end, beg - end);
}

static void mark(MSHeap *heap)
{
    mark_roots(heap->roots);
    mark_stack();
    jmp_buf jmp;
    setjmp(jmp);
    mark_jmpbuf(&jmp);
}

static void ms_gc(MSHeap *heap)
{
    static bool in_gc = false;
    if (in_gc)
        bug("nested GC detected");
    in_gc = true;
    if (print_stat)
        heap_print_stat("GC begin");
    mark(heap);
    sweep(heap);
    if (print_stat)
        heap_print_stat("GC end");
    in_gc = false;
}

static void *ms_malloc(size_t size)
{
    MSHeap *heap = gc_data;
    if (stress)
        ms_gc(heap);
    void *p = ms_allocate(heap, size);
    if (!stress && p == NULL) {
        ms_gc(heap);
        p = ms_allocate(heap, size);
    }
    if (p == NULL) {
        ms_add_slot(heap);
        p = ms_allocate(heap, size);
    }
    if (p == NULL)
        error_out_of_memory();
    return p;
}

static void ms_stat_slot(HeapStat *stat, const MSHeapSlot *slot)
{
    stat->size += slot->size;
    const MSHeader *h;
    for (const uint8_t *p = slot->body, *endp = p + slot->size; p < endp; p += HSIZE(h->size)) {
        h = MS_HEADER(p);
        size_t j = h->size - 1;
        if (j > TABMAX) {
            size_t **tab = h->used ? &stat->tab_used_larger : &stat->tab_free_larger;
            scary_push(tab, h->size);
            continue;
        }
        if (h->used) {
            stat->used += h->size;
            stat->tab_used[j]++;
        } else
            stat->tab_free[j]++;
    }
}

static void ms_stat(HeapStat *stat)
{
    MSHeap *heap = gc_data;
    for (size_t i = 0; i < heap->size; i++)
        ms_stat_slot(stat, heap->slot[i]);
}

#define free_slots(slot, size) do { \
        for (size_t i = 0; i < size; i++) { \
            free(slot[i]->body); \
            free(slot[i]); \
        } \
    } while (0)

static void ms_fin(void)
{
    MSHeap *heap = gc_data;
    sweep(heap); // nothing marked, all values are freed
    free_slots(heap->slot, heap->size);
    scary_free(heap->roots);
    free(heap);
}

static const GCFunctions GC_FUNCS_MARK_SWEEP = {
    .init = ms_init,
    .fin = ms_fin,
    .malloc = ms_malloc,
    .add_root = ms_add_root,
    .stat = ms_stat
};
static const GCFunctions GC_FUNCS_DEFAULT = GC_FUNCS_MARK_SWEEP;

//
// Algorithm: Mark-and-sweep + Bitmap Marking
//

static uintptr_t bitmap_index(const void *p)
{
    MSHeap *heap = gc_data;
    return ptrdiff_abs(p, heap->low) / PTR_ALIGN;
}

static void bmp_init_bitmap(void)
{
    MSHeap *heap = gc_data;
    size_t rawsize = bitmap_index(heap->high);
    heap->bitmap = xcalloc(1, idivceil(rawsize, 8U));
}

static void bmp_gc(MSHeap *heap)
{
    bmp_init_bitmap();
    ms_gc(heap);
}

static void *bmp_malloc(size_t size)
{
    MSHeap *heap = gc_data;
    if (stress)
        bmp_gc(heap);
    void *p = ms_allocate(heap, size);
    if (!stress && p == NULL) {
        bmp_gc(heap);
        p = ms_allocate(heap, size);
    }
    if (p == NULL) {
        ms_add_slot(heap);
        p = ms_allocate(heap, size);
    }
    if (p == NULL)
        error_out_of_memory();
    return p;
}

// We use ms_fin() **as is** even in the bitmap-marking mode.
// It seems dangerous because we don't provide heap->bitmap for the last
// sweep() in fin(), but we can do it safely in fact. It depends on the
// behavior of init_header() which sets every header->living = false.
static const GCFunctions GC_FUNCS_MARK_SWEEP_BITMAP = {
    .init = ms_init,
    .fin = ms_fin,
    .malloc = bmp_malloc,
    .add_root = ms_add_root,
    .stat = ms_stat
};

//
// Algorithm: Epsilon
//

typedef struct {
    size_t size, used;
    uint8_t *body;
} EpsHeapSlot;

typedef struct {
    size_t size;
    EpsHeapSlot *slot[64];
} EpsHeap;

static EpsHeapSlot *eps_heap_slot_new(size_t size)
{
    EpsHeapSlot *h = xmalloc(sizeof(EpsHeapSlot));
    h->size = align(size);
    h->used = 0;
    h->body = xmalloc(size);
    return h;
}

static void eps_init(void)
{
    EpsHeap *heap = xmalloc(sizeof(EpsHeap));
    heap->slot[0] = eps_heap_slot_new(init_size);
    heap->size = 1;
    gc_data = heap;
}

static void eps_fin(void)
{
    EpsHeap *heap = gc_data;
    free_slots(heap->slot, heap->size);
    free(heap);
}

static inline EpsHeapSlot *last_slot(const EpsHeap *heap)
{
    return heap->slot[heap->size - 1];
}

static void *eps_allocate(EpsHeap *heap, size_t size)
{
    EpsHeapSlot *last = last_slot(heap); // use the last slot only
    if (last->used + size > last->size)
        return NULL;
    uint8_t *ret = last->body + last->used;
    last->used += size;
    return ret;
}

static void eps_add_slot(EpsHeap *heap)
{
    EpsHeapSlot *last = last_slot(heap);
    heap->slot[heap->size++] = eps_heap_slot_new(last->size * HEAP_RATIO);
}

static void *eps_malloc(size_t size)
{
    EpsHeap *heap = gc_data;
    void *p = eps_allocate(heap, size);
    if (p == NULL) {
        eps_add_slot(heap);
        p = eps_allocate(heap, size);
    }
    if (p == NULL)
        error_out_of_memory();
    return p;
}

static void eps_add_root(UNUSED const Value *p) {} // dummy

static void eps_stat(HeapStat *stat)
{
    EpsHeap *heap = gc_data;
    stat->size = stat->used = 0;
    for (size_t i = 0; i < heap->size; i++) {
        EpsHeapSlot *slot = heap->slot[i];
        stat->size += slot->size;
        stat->used += slot->used;
    }
}

static const GCFunctions GC_FUNCS_EPSILON = {
    .init = eps_init,
    .fin = eps_fin,
    .malloc = eps_malloc,
    .add_root = eps_add_root,
    .stat = eps_stat
};

//
// General functions
//

void gc_add_root(const Value *r)
{
    funcs.add_root(r);
}

static void heap_print_stat_table(const char *subheader, size_t tab[], const size_t *large)
{
    debug("%s:", subheader);
    for (size_t i = 0; i < TABMAX; i++) {
        if (tab[i] > 0)
            fprintf(stderr, "    [%5zu] %zu\n", i+1, tab[i]);
    }
    size_t len = scary_length(large);
    if (len == 0)
        return;
    fprintf(stderr, "    [>%d] %zu ", TABMAX, len);
    fprintf(stderr, "'(%zu", large[0]);
    for (size_t i = 1; i < len; i++) {
        fprintf(stderr, " %zu", large[i]);
    }
    fprintf(stderr, ")\n");
}

static void heap_stat(HeapStat *stat)
{
    memset(stat, 0, sizeof(HeapStat));
    stat->tab_free_larger = scary_new(sizeof(size_t));
    stat->tab_used_larger = scary_new(sizeof(size_t));
    funcs.stat(stat);
}

static void heap_stat_fin(HeapStat *stat)
{
    scary_free(stat->tab_free_larger);
    scary_free(stat->tab_used_larger);
}

static void heap_print_stat(const char *header)
{
    if (header != NULL)
        debug("%s:", header);
    HeapStat stat;
    heap_stat(&stat);
    int n = ceil(log10(stat.size));
    long r = lround(((double) stat.used / stat.size) * 1000);
    debug("heap usage: %*zu / %*zu (%3ld.%1ld%%)",
          n, stat.used, n, stat.size, r/10, r%10);
    heap_print_stat_table("used", stat.tab_used, stat.tab_used_larger);
    heap_print_stat_table("free", stat.tab_free, stat.tab_free_larger);
    heap_stat_fin(&stat);
    debug("");
}

static size_t heap_size(void)
{
    HeapStat stat;
    heap_stat(&stat);
    size_t size = stat.size;
    heap_stat_fin(&stat);
    return size;
}

static void error_out_of_memory(void)
{
    error("out of memory; heap (~%zu MiB) exhausted",
          (size_t) round((double) heap_size() / MiB));
}

#define error_if_gc_initialized() \
    if (initialized) error("%s called after GC initialization", __func__)

void sch_set_gc_init_size(double init_mib)
{
    error_if_gc_initialized();
    init_size = round(init_mib * MiB);
}

void sch_set_gc_stress(bool b)
{
    stress = b;
}

void sch_set_gc_print_stat(bool b)
{
    print_stat = b;
}

void sch_set_gc_algorithm(SchGCAlgorithm s)
{
    error_if_gc_initialized();
    switch (s) {
    case SCH_GC_ALGORITHM_EPSILON:
        funcs = GC_FUNCS_EPSILON;
        break;
    case SCH_GC_ALGORITHM_MARK_SWEEP:
        funcs = GC_FUNCS_MARK_SWEEP;
        break;
    case SCH_GC_ALGORITHM_MARK_SWEEP_BITMAP:
        funcs = GC_FUNCS_MARK_SWEEP_BITMAP;
        break;
    }
}

void gc_init(const void *sp)
{
    stack_base = sp;
    if (funcs.init == NULL)
        funcs = GC_FUNCS_DEFAULT;
    funcs.init();
    initialized = true;
}

void gc_fin(void)
{
    funcs.fin();
}

void *gc_malloc(size_t size)
{
    size_t asize = align(size);
    void *p = funcs.malloc(asize);
#ifdef DEBUG
    memset(p, 0, size);
#endif
    return p;
}

size_t gc_stack_get_size(const void *sp)
{
    return ptrdiff_abs(stack_base, sp);
}
