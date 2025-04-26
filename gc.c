#include <math.h>
#include <stdbool.h>
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
};

typedef struct {
    size_t size, used;
    size_t tab_free[TABMAX+1], tab_used[TABMAX+1];
} HeapStat;

typedef struct {
    void (*init)(const uintptr_t *volatile sp);
    void (*fin)(void);
    void *(*malloc)(size_t size);
    void (*add_root)(const Value *r);
    void (*stat)(HeapStat *stat);
} GCFunctions;

static void heap_print_stat(const char *header);
static size_t heap_size(void);
[[gnu::noreturn]] static void error_out_of_memory(void);
static inline size_t align(size_t size);

// Static data
static void *gc_data; // singleton; maybe a heap or some context
static GCFunctions funcs;
static size_t init_size = 1 * MiB;

static const uintptr_t *volatile stack_base;

static bool stress, print_stat;
static bool in_gc, initialized;

//
// Algorithm: Mark-and-sweep
//

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
    Header *free_list;
} MSHeap;

static void init_chunk(Header *h, size_t size, Header *next)
{
    h->living = false;
    // h->immutable = false;
    h->size = size;
    h->tag = TAG_CHUNK;
    HEADER_NEXT(h) = next;
}

#define MIN(x, y) ((x) < (y) ? (x) : (y))
static MSHeapSlot *ms_heap_slot_new(size_t size)
{
    MSHeapSlot *s = xmalloc(sizeof(MSHeapSlot));
    size = align(size);
    s->size = size;
    s->body = xmalloc(size);
    Header *h = HEADER(s->body);
    memset(h, 0, MIN(size, sizeof(SchObject))); // XXX
    init_chunk(h, size, NULL);
    return s;
}

static void ms_init(const uintptr_t *volatile sp)
{
    stack_base = sp;
    MSHeap *heap = xmalloc(sizeof(MSHeap));
    heap->roots = scary_new(sizeof(Value *));
    heap->slot[0] = ms_heap_slot_new(init_size);
    MSHeapSlot *first = heap->slot[0];
    heap->size = 1;
    heap->low = first->body;
    heap->high = heap->low + first->size;
    heap->free_list = HEADER(first->body);
    gc_data = heap;
}

static void ms_add_root(const Value *r)
{
    MSHeap *heap = gc_data;
    scary_push((void ***) &heap->roots, (void *) r);
}

bool in_heap_range(volatile uintptr_t v)
{
    MSHeap *heap = gc_data;
    const uint8_t *volatile p = (uint8_t *volatile) v;
    return p >= heap->low && p < heap->high;
}

static bool is_valid_tag(ValueTag t)
{
    return t < TAG_CHUNK;
}

static bool is_valid_pointer(Value v)
{
    return v % 16U == 0 && v > 0; // XXX
}

static bool is_valid_header(Value v)
{
    Header *h = HEADER(v);
    UNPOISON(&h->tag, sizeof(ValueTag)); // Suspicious but
    UNPOISON(&h->size, sizeof(size_t));  // need to be read
    return is_valid_tag(h->tag) &&
        h->size >= sizeof(SchObject) && h->size < sizeof(SchObject) * 2;
}

static bool in_heap_val(Value v)
{
    return is_valid_pointer(v) && in_heap_range(v) && is_valid_header(v);
}

static void mark_val(Value v);

static void mark_array(const void *volatile beg, size_t n)
{
    UNPOISON(beg, n * sizeof(uintptr_t));
    const Value *volatile p = beg;
    for (size_t i = 0; i < n; i++, p++) {
        if (in_heap_val(*p))
            mark_val(*p);
    }
}

static void mark_jmpbuf(const jmp_buf *jmp)
{
    mark_array(jmp, (size_t) sizeof(jmp_buf) / sizeof(uintptr_t));
}

static void mark_env_each(UNUSED uint64_t key, uint64_t value)
{
    // key is always a Symbol
    mark_val(value);
}

static void mark_val(Value v)
{
    if (!is_valid_pointer(v))
        return;
    Header *h = HEADER(v);
    if (h->living)
        return;
    h->living = true; // mark it!
    switch (VALUE_TAG(v)) {
    case TAG_PAIR: {
        Pair *p = PAIR(v);
        mark_val(p->car);
        mark_val(p->cdr);
        break;
    }
    case TAG_CLOSURE: {
        Closure *p = CLOSURE(v);
        mark_val(p->env);
        mark_val(p->params);
        mark_val(p->body);
        break;
    }
    case TAG_CONTINUATION: {
        Continuation *p = CONTINUATION(v);
        mark_val(p->retval);
        mark_jmpbuf(&p->exstate->regs);
        mark_array(p->exstate->stack, p->exstate->stack_len / sizeof(uintptr_t));
        break;
    }
    case TAG_CFUNC_CLOSURE:
        mark_val(CFUNC_CLOSURE(v)->data);
        break;
    case TAG_ENV: {
        Env *p = ENV(v);
        table_foreach(p->table, mark_env_each);
        mark_val(p->parent);
        break;
    }
    case TAG_VECTOR: {
        Value *p = VECTOR(v);
        for (int64_t i = 0, len = scary_length(p); i < len; i++)
            mark_val(p[i]);
        break;
    }
    case TAG_HSTRING:
    case TAG_ESTRING:
    case TAG_CFUNC:
    case TAG_SYNTAX:
    case TAG_PORT:
    case TAG_EOF:
    case TAG_ERROR:
        break;
    case TAG_CHUNK:
        UNREACHABLE();
    }
}

static Header *allocate_from_chunk(Header *prev, Header *curr, size_t size)
{
    MSHeap *heap = gc_data;
    Header *next = HEADER_NEXT(curr);
    if (curr->size > size + sizeof(Header)) {
        Header *rest = (Header *)((uint8_t *) curr + size);
        init_chunk(rest, curr->size - size, next);
        next = rest;
        curr->size = size;
    }
    if (prev == NULL)
        heap->free_list = next;
    else
        HEADER_NEXT(prev) = next;
    return curr;
}

static void add_to_free_list(Header *h)
{
    MSHeap *heap = gc_data;
    HEADER_NEXT(h) = heap->free_list; // prepend
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
    case TAG_CONTINUATION: {
        Continuation *p = CONTINUATION(v);
        free(p->exstate->stack);
        free(p->exstate);
        break;
    }
    case TAG_HSTRING:
        free(HSTRING(v));
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
    case TAG_ERROR:
        scary_free(ERROR(v));
        break;
    case TAG_CFUNC:
    case TAG_SYNTAX:
    case TAG_CFUNC_CLOSURE:
    case TAG_CLOSURE:
    case TAG_ESTRING:
    case TAG_PAIR:
    case TAG_EOF:
        break;
    case TAG_CHUNK:
        UNREACHABLE();
    }
}
#ifdef DEBUG
#undef cfree
#undef free
#undef table_free
#undef scary_free
#endif

static bool adjoining_p(const Header *curr)
{
    MSHeap *heap = gc_data;
    Header *free_list = heap->free_list;
    return free_list != NULL &&
        (uint8_t *) free_list + free_list->size == (void *) curr;
}

static void free_chunk(Header *curr)
{
    MSHeap *heap = gc_data;
    Header *free_list = heap->free_list;
    Value val = (Value) curr;
    free_val(val);
    if (adjoining_p(curr)) {
        free_list->size += curr->size;
#ifdef DEBUG
        memset(curr, 0, sizeof(SchObject));
#else
        curr->size = 0; // XXX: for is_valid_header ?
#endif
        return;
    }
    curr->tag = TAG_CHUNK;
    add_to_free_list(curr);
}

static void sweep_slot(MSHeapSlot *slot)
{
    uint8_t *p = slot->body, *endp = p + slot->size;
    size_t offset;
    for (Header *h; p < endp; p += offset) {
        h = HEADER(p);
        offset = h->size;
        if (h->tag == TAG_CHUNK)
            continue;
        if (h->living)
            h->living = false;
        else
            free_chunk(h);
    }
}

static void sweep(void)
{
    MSHeap *heap = gc_data;
    for (size_t i = 0; i < heap->size; i++)
        sweep_slot(heap->slot[i]);
}

static void ms_add_slot(void)
{
    MSHeap *heap = gc_data;
    MSHeapSlot *last = heap->slot[heap->size - 1];
    size_t new_size = last->size * HEAP_RATIO;
    last = heap->slot[heap->size++] = ms_heap_slot_new(new_size);
    uint8_t *beg = last->body, *end = beg + last->size;
    if (heap->low > beg)
        heap->low = beg;
    else if (heap->high < end)
        heap->high = end;
    add_to_free_list(HEADER(beg));
}

static Header *ms_allocate(size_t size)
{
    MSHeap *heap = gc_data;
    Header *free_list = heap->free_list;
    for (Header *prev = NULL, *curr = free_list; curr != NULL; prev = curr, curr = HEADER_NEXT(curr)) {
        if (curr->size >= size) // First-fit
            return allocate_from_chunk(prev, curr, size);
    }
    return NULL;
}

static void mark_roots(void)
{
    MSHeap *heap = gc_data;
    for (size_t i = 0, len = scary_length(heap->roots); i < len; i++)
        mark_val(*heap->roots[i]);
}

[[gnu::noinline]]
static void mark_stack(void)
{
    GET_SP(sp);
    mark_array(sp, stack_base - sp);
}

static void mark(void)
{
    mark_stack();
    mark_roots();
    jmp_buf jmp;
    setjmp(jmp);
    mark_jmpbuf(&jmp);
}

static void ms_gc(void)
{
    if (UNLIKELY(in_gc))
        bug("nested GC detected");
    in_gc = true;
    if (print_stat)
        heap_print_stat("GC begin");
    mark();
    sweep();
    if (print_stat)
        heap_print_stat("GC end");
    in_gc = false;
}

static void *ms_malloc(size_t size)
{
    if (stress)
        ms_gc();
    size = align(size);
    Header *p = ms_allocate(size);
    if (!stress && p == NULL) {
        ms_gc();
        p = ms_allocate(size);
    }
    if (p == NULL) {
        ms_add_slot();
        p = ms_allocate(size);
    }
    if (UNLIKELY(p == NULL))
        error_out_of_memory();
#ifdef DEBUG
    memset((uint8_t *) p + sizeof(Header), 0, size - sizeof(Header));
#endif
    return p;
}

static void ms_stat(HeapStat *stat)
{
    MSHeap *heap = gc_data;
    memset(stat->tab_free, 0, sizeof(stat->tab_free));
    memset(stat->tab_used, 0, sizeof(stat->tab_used));
    for (size_t i = 0; i < heap->size; i++) {
        MSHeapSlot *slot = heap->slot[i];
        stat->size += slot->size;
        Header *h;
        for (uint8_t *p = slot->body, *endp = p + slot->size; p < endp; p += h->size) {
            h = HEADER(p);
            size_t j = h->size - 1;
            if (j > TABMAX)
                j = TABMAX;
            if (h->tag == TAG_CHUNK)
                stat->tab_free[j]++;
            else {
                stat->used += h->size;
                stat->tab_used[j]++;
            }
        }
    }
}

static void ms_fin(void)
{
    MSHeap *heap = gc_data;
    sweep(); // nothing marked, all values are freed
    for (size_t i = 0; i < heap->size; i++) {
        free(heap->slot[i]->body);
        free(heap->slot[i]);
    }
    scary_free(heap->roots);
    free(heap);
}

static const GCFunctions GC_FUNCS_MARK_SWEEP = {
    ms_init, ms_fin, ms_malloc, ms_add_root, ms_stat
};
static const GCFunctions GC_FUNCS_DEFAULT = GC_FUNCS_MARK_SWEEP;

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

static inline size_t align(size_t size)
{
    return (size + 15U) / 16U * 16U;
}

static EpsHeapSlot *eps_heap_slot_new(size_t size)
{
    EpsHeapSlot *h = xmalloc(sizeof(EpsHeapSlot));
    h->size = align(size);
    h->used = 0;
    h->body = xmalloc(size);
    return h;
}

static void eps_init(const uintptr_t *volatile sp)
{
    stack_base = sp;
    EpsHeap *heap = xmalloc(sizeof(EpsHeap));
    heap->slot[0] = eps_heap_slot_new(init_size);
    heap->size = 1;
    gc_data = heap;
}

static void eps_fin(void)
{
    EpsHeap *heap = gc_data;
    for (size_t i = 0; i < heap->size; i++) {
        free(heap->slot[i]->body);
        free(heap->slot[i]);
    }
    free(heap);
}

static inline EpsHeapSlot *last_slot(EpsHeap *heap)
{
    return heap->slot[heap->size - 1];
}

static void *eps_allocate(size_t size)
{
    EpsHeap *heap = gc_data;
    EpsHeapSlot *last = last_slot(heap); // use the last slot only
    if (last->used + size > last->size)
        return NULL;
    uint8_t *ret = last->body + last->used;
    last->used += size;
    return ret;
}

static bool eps_has_minimum_free_space(void)
{
    EpsHeap *heap = gc_data;
    static const size_t minreq = sizeof(Continuation); // maybe the largest
    EpsHeapSlot *last = last_slot(heap);
    return (last->size - last->used) >= minreq;
}

static void eps_increase_heap(void)
{
    EpsHeap *heap = gc_data;
    EpsHeapSlot *last = last_slot(heap);
    heap->slot[heap->size++] = eps_heap_slot_new(last->size * HEAP_RATIO);
}

static void eps_add_root(UNUSED const Value *p) {} // dummy

static void eps_stat(HeapStat *stat)
{
    EpsHeap *heap = gc_data;
    memset(stat->tab_free, 0, sizeof(stat->tab_free));
    memset(stat->tab_used, 0, sizeof(stat->tab_used));
    stat->size = stat->used = 0;
    for (size_t i = 0; i < heap->size; i++) {
        EpsHeapSlot* slot = heap->slot[i];
        stat->size += slot->size;
        stat->used += slot->used;
    }
}

static void eps_gc(void)
{
    if (UNLIKELY(in_gc))
        bug("nested GC detected");
    in_gc = true;
    if (print_stat)
        heap_print_stat("GC begin");
    if (!eps_has_minimum_free_space())
        eps_increase_heap(); // collects nothing ;)
    if (print_stat)
        heap_print_stat("GC end");
    in_gc = false;
}

static void *eps_malloc(size_t size)
{
    if (stress)
        eps_gc();
    size = align(size);
    void *p = eps_allocate(size);
    if (!stress && p == NULL) {
        eps_gc();
        p = eps_allocate(size);
    }
    if (UNLIKELY(p == NULL))
        error_out_of_memory();
    return p;
}

static const GCFunctions GC_FUNCS_EPSILON = {
    eps_init, eps_fin, eps_malloc, eps_add_root, eps_stat
};

//
// General functions
//

static void error_out_of_memory(void)
{
    error("out of memory; heap (~%zu MiB) exhausted", heap_size() / MiB);
}

void gc_add_root(const Value *r)
{
    funcs.add_root(r);
}

static void heap_print_stat_table(const char *header, size_t tab[])
{
    debug("%s:", header);
    for (size_t i = 0; i < TABMAX; i++) {
        if (tab[i] > 0)
            fprintf(stderr, "    [%5zu] %zu\n", i+1, tab[i]);
    }
    if (tab[TABMAX] > 0)
        fprintf(stderr, "    [>%d] %zu\n", TABMAX, tab[TABMAX]);
}

static void heap_stat(HeapStat *stat)
{
    funcs.stat(stat);
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
    heap_print_stat_table("used", stat.tab_used);
    heap_print_stat_table("free", stat.tab_free);
    debug("");
}

static size_t heap_size(void)
{
    HeapStat stat;
    heap_stat(&stat);
    return stat.size;
}

UNUSED
static size_t heap_used(void)
{
    HeapStat stat;
    heap_stat(&stat);
    return stat.used;
}

#define error_if_gc_initialized() if (initialized) error("%s called after GC initialization", __func__)

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
    case GC_ALGORITHM_EPSILON:
        funcs = GC_FUNCS_EPSILON;
        break;
    case GC_ALGORITHM_MARK_SWEEP:
        funcs = GC_FUNCS_MARK_SWEEP;
        break;
    default:
        UNREACHABLE();
    }
}

void gc_init(const uintptr_t *volatile sp)
{
    if (funcs.init == NULL)
        funcs = GC_FUNCS_DEFAULT;
    funcs.init(sp);
    initialized = true;
}

void gc_fin(void)
{
    funcs.fin();
}

void *gc_malloc(size_t size)
{
    return funcs.malloc(size);
}

size_t gc_stack_get_size(const uintptr_t *volatile sp)
{
    return (uint8_t *volatile) stack_base - (uint8_t *volatile) sp;
}
