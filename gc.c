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

enum {
    MiB = 1024 * 1024,
    HEAP_RATIO = 2,
    TABMAX = 1024,
    ROOT_SIZE = 0x10,
};

typedef struct {
    size_t size, used;
    uint8_t *body;
} HeapSlot;

typedef struct {
    size_t size;
    // 64 is enough large, it can use up the entire 64-bit memory space
    // (= (fold + 0 (map (cut expt 2 <>) (iota 64))) (- (expt 2 64) 1)) ;;=> #t
    HeapSlot *slot[64];
} Heap;

typedef struct {
    size_t size, used;
    size_t tab_free[TABMAX+1], tab_used[TABMAX+1];
} HeapStat;

static size_t init_size = 1 * MiB;
static Heap heap; // singleton
static uint8_t *heap_low, *heap_high;

static Header *free_list;

static uintptr_t *volatile stack_base;
static const Value *roots[ROOT_SIZE];
static size_t nroot;

static bool stress, print_stat;
static bool in_gc;

ATTR_XMALLOC static void *ms_gc_malloc(size_t size);
ATTR_XMALLOC static void *eps_gc_malloc(size_t size);

void *(*gc_malloc)(size_t size) = ms_gc_malloc;

void sch_set_gc_init_size(double init_mib)
{
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

void sch_set_gc_strategy(SchGCStrategy s)
{
    switch (s) {
    case GC_STRATEGY_MARK_SWEEP:
        gc_malloc = ms_gc_malloc;
        break;
    case GC_STRATEGY_EPSILON:
        gc_malloc = eps_gc_malloc;
        break;
    default:
        UNREACHABLE();
    }
}

static inline size_t align(size_t size)
{
    const size_t n = sizeof(SchObject);
    return (size + (n-1)) / n * n;
}

#define MIN(x, y) ((x) < (y) ? (x) : (y))
static void init_chunk(Header *h, size_t size)
{
    h->living = false;
    // h->immutable = false;
    h->size = size;
    h->tag = TAG_CHUNK;
}

#ifdef DEBUG
#define xmalloc(n) xcalloc(1, (n))
#endif

static HeapSlot *heap_slot_new(size_t size)
{
    HeapSlot *s = xmalloc(sizeof(HeapSlot));
    size = align(size);
    s->size = size;
    s->body = xmalloc(size);
#if defined(__clang__) && __clang_major__ < 19 // XXX: ???
    memset(s->body, 0, MIN(size, sizeof(Header)));
#endif
    Header *h = HEADER(s->body);
    init_chunk(h, size);
    HEADER_NEXT(h) = NULL;
    return s;
}

void gc_init(uintptr_t *volatile sp)
{
    stack_base = sp;
    // init_size = align(init_size);
    heap.slot[0] = heap_slot_new(init_size);
    HeapSlot *first = heap.slot[0];
    heap.size = 1;
    heap_low = first->body;
    heap_high = heap_low + first->size;
    free_list = HEADER(first->body);
}

// Allocation

static Header *allocate_from_chunk(Header *prev, Header *curr, size_t size)
{
    Header *next = HEADER_NEXT(curr);
    if (curr->size > size) {
        Header *rest = (Header *)((uint8_t *) curr + size);
        init_chunk(rest, curr->size - size);
        HEADER_NEXT(rest) = next;
        next = rest;
        curr->size = size;
    }
    if (prev == NULL)
        free_list = next;
    else
        HEADER_NEXT(prev) = next;
    return curr;
}

static Header *allocate(size_t size)
{
    for (Header *prev = NULL, *curr = free_list; curr != NULL; prev = curr, curr = HEADER_NEXT(curr)) {
        if (curr->size >= size) // First-fit
            return allocate_from_chunk(prev, curr, size);
    }
    return NULL;
}

size_t gc_stack_get_size(uintptr_t *volatile sp)
{
    return (uint8_t *volatile) stack_base - (uint8_t *volatile) sp;
}

// Marking

static void mark_val(Value v);

void sch_gc_mark(Value v)
{
    mark_val(v);
}

#if 0
static bool in_heap_slot(const HeapSlot *slot, const uint8_t *p)
{
    const uint8_t *beg = slot->body, *end = beg + slot->size;
    return p >= beg && p < end;
}
#endif

bool in_heap_range(volatile uintptr_t v)
{
    const uint8_t *volatile p = (uint8_t *volatile) v;
    if (p < heap_low || p >= heap_high)
        return false;
#if 0// left for a while
    for (size_t i = 0; i < heap.size; i++) {
        if (in_heap_slot(heap.slot[i], p))
            return true;
    }
    return false;
#else
    return true;
#endif
}

static bool is_valid_tag(ValueTag t)
{
    return t < TAG_CHUNK;
}

static bool is_valid_pointer(Value v)
{
    return v % 16U == 0 && v >= 0x100000;
}

static bool is_valid_header(Value v)
{
    Header *h = HEADER(v);
    return is_valid_tag(h->tag) && h->size == sizeof(SchObject);
}

static bool in_heap_val(Value v)
{
    return is_valid_pointer(v) && in_heap_range(v) && is_valid_header(v);
}

static void mark_array(void *volatile beg, size_t n)
{
    UNPOISON(beg, n * sizeof(uintptr_t));
    Value *volatile p = beg;
    for (size_t i = 0; i < n; i++, p++) {
        if (in_heap_val(*p))
            mark_val(*p);
    }
}

static void mark_jmpbuf(jmp_buf *jmp)
{
    mark_array(&jmp, (size_t) sizeof(jmp_buf) / sizeof(uintptr_t));
}

static void mark_env_each(uint64_t key, uint64_t value, UNUSED void *data)
{
    mark_val(key);
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
        return;
    }
    case TAG_CLOSURE: {
        Closure *p = CLOSURE(v);
        mark_val(p->env);
        mark_val(p->params);
        mark_val(p->body);
        return;
    }
    case TAG_CONTINUATION: {
        Continuation *p = CONTINUATION(v);
        mark_val(p->retval);
        mark_jmpbuf(&p->exstate->regs);
        mark_array(p->exstate->stack, p->exstate->stack_len / sizeof(uintptr_t));
        return;
    }
    case TAG_CFUNC_CLOSURE:
        mark_val(CFUNC_CLOSURE(v)->data);
        return;
    case TAG_ENV: {
        Env *p = ENV(v);
        table_foreach(p->table, mark_env_each, NULL);
        mark_val(p->parent);
        return;
    }
    case TAG_VECTOR: {
        Value *p = VECTOR(v);
        for (int64_t i = 0, len = scary_length(p); i < len; i++)
            mark_val(p[i]);
        return;
    }
    case TAG_HSTRING:
    case TAG_ESTRING:
    case TAG_CFUNC:
    case TAG_SYNTAX:
    case TAG_PORT:
    case TAG_EOF:
    case TAG_ERROR:
        return;
    case TAG_CHUNK:
        UNREACHABLE();
    }
}

void gc_add_root(const Value *r)
{
    if (nroot == ROOT_SIZE)
        error("%s: too many roots added", __func__);
    roots[nroot++] = r;
}

static void mark_roots(void)
{
    for (size_t i = 0; i < nroot; i++)
        mark_val(*roots[i]);
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
    memset(&jmp, 0, sizeof(jmp_buf));
    setjmp(jmp);
    mark_jmpbuf(&jmp);
}

// Dump for debug

UNUSED
static bool chunk_header_equal(Header *a, Header *b)
{
    return a->size == b->size;
}

UNUSED
static void heap_slot_dump(const HeapSlot *slot)
{
    uint8_t *p = slot->body, *endp = p + init_size;
    fprintf(stderr, "begin: %p..%p\n", p, endp);
    bool ellipsis = false;
    for (Header *h, *prev = NULL; p < endp; p += h->size, prev = h) {
        h = HEADER(p);
        if (prev != NULL && chunk_header_equal(h, prev)) {
            if (!ellipsis) {
                fprintf(stderr, "  [..]\n");
                ellipsis = true;
            }
            continue;
        }
        ellipsis = false;
        fprintf(stderr, "  [%p] size: %2zu\n", h, h->size);
    }
    fprintf(stderr, "end: %p..%p\n", p, endp);
}

UNUSED
static void heap_dump(void)
{
    for (size_t i = 0; i < heap.size; i++)
        heap_slot_dump(heap.slot[i]);
}

static void heap_stat(HeapStat *stat)
{
    stat->size = stat->used = 0;
    memset(stat->tab_free, 0, sizeof(stat->tab_free));
    memset(stat->tab_used, 0, sizeof(stat->tab_used));
    for (size_t i = 0; i < heap.size; i++) {
        HeapSlot *slot = heap.slot[i];
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

static void heap_print_stat_table(size_t tab[])
{
    for (size_t i = 0; i < TABMAX; i++) {
        if (tab[i] > 0)
            fprintf(stderr, "    [%5zu] %zu\n", i+1, tab[i]);
    }
    if (tab[TABMAX] > 0)
        fprintf(stderr, "    [>%d] %zu\n", TABMAX, tab[TABMAX]);
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
    debug("used:");
    heap_print_stat_table(stat.tab_used);
    debug("free:");
    heap_print_stat_table(stat.tab_free);
    debug("");
}

// Freeing

static void add_to_free_list(Header *h)
{
    HEADER_NEXT(h) = free_list; // prepend
    free_list = h;
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
    case TAG_CFUNC:
    case TAG_SYNTAX:
    case TAG_CFUNC_CLOSURE: {
        CFunc *p = CFUNC(v);
        free(p->name);
        break;
    }
    case TAG_CONTINUATION: {
        Continuation *p = CONTINUATION(v);
        free(p->exstate->stack);
        free(p->exstate);
        break;
    }
    case TAG_HSTRING:
        free(HSTRING(v));
        break;
    case TAG_ENV: {
        Env *p = ENV(v);
        table_free(p->table);
        free(p->name);
        break;
    }
    case TAG_PORT: {
        FILE *fp = PORT(v)->fp;
        if (is_user_opened_file(fp))
            fclose(fp);
        PORT(v)->fp = NULL; // keep safety for fclose()
        break;
    }
    case TAG_VECTOR: {
        scary_free(VECTOR(v));
        break;
    }
    case TAG_ERROR: {
        scary_free(ERROR(v));
        break;
    }
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
    return free_list != NULL &&
        (uint8_t *) free_list + free_list->size == (void *) curr;
}

static void free_chunk(Header *curr)
{
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

static void sweep_slot(HeapSlot *slot)
{
    uint8_t *p = slot->body, *endp = p + slot->size;
    size_t offset;
    for (Header *h; p < endp; p += offset) {
        h = HEADER(p);
        offset = h->size;
        if (h->tag == TAG_CHUNK)
            continue;
        if (h->living) {
            h->living = false;
            continue;
        }
        free_chunk(h);
    }
}

static void sweep(void)
{
    for (size_t i = 0; i < heap.size; i++)
        sweep_slot(heap.slot[i]);
}

static void add_slot(void)
{
    HeapSlot *last = heap.slot[heap.size-1];
    size_t new_size = last->size * HEAP_RATIO;
    last = heap.slot[heap.size++] = heap_slot_new(new_size);
    uint8_t *beg = last->body, *end = beg + last->size;
    if (heap_low > beg)
        heap_low = beg;
    if (heap_high < end)
        heap_high = end;
    Header *h = HEADER(last->body);
    add_to_free_list(h);
}

UNUSED
static size_t heap_used(void)
{
    HeapStat stat;
    heap_stat(&stat);
    return stat.used;
}

// Entry: gc() and gc_malloc()

static void gc(void)
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

static size_t heap_size(void)
{
    HeapStat stat;
    heap_stat(&stat);
    return stat.size;
}

static void *eps_gc_malloc(size_t size)
{
    HeapSlot *last = heap.slot[heap.size-1]; // use the last slot only
    if (last->used + size > last->size)
        return NULL;
    uint8_t *ret = last->body + last->used;
    last->used += size;
    return ret;
}

void *ms_gc_malloc(size_t size)
{
    if (stress)
        gc();
    size = align(size);
    Header *p = allocate(size);
    if (!stress && p == NULL) {
        gc();
        p = allocate(size);
    }
    if (p == NULL) {
        add_slot();
        p = allocate(size);
    }
    if (UNLIKELY(p == NULL))
        error("unreachable: heap (~%zu MiB) exhausted", heap_size() / MiB);
#ifdef DEBUG
    memset((uint8_t *) p + sizeof(Header), 0, size - sizeof(Header));
#endif
    return p;
}

void gc_fin(void)
{
    sweep(); // nothing marked, all values are freed
    for (size_t i = 0; i < heap.size; i++) {
        free(heap.slot[i]->body);
        free(heap.slot[i]);
    }
}
