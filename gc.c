#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "intern.h"
#include "libscary.h"
#include "utils.h"

enum {
    MiB = 1024 * 1024,
    HEAP_RATIO = 2,
    TABMAX = 1024,
    ROOT_SIZE = 0x08,
};

typedef struct {
    size_t size;
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

static inline size_t align(size_t size)
{
    return (size + 7U) / 8U * 8U;
}

#define MIN(x, y) ((x) < (y) ? (x) : (y))
static void init_chunk(Header *h, size_t size)
{
    h->living = false;
    h->immutable = false;
    h->size = size;
    h->tag = TAG_CHUNK;
}

static HeapSlot *heap_slot_new(size_t size)
{
    HeapSlot *h = xmalloc(sizeof(HeapSlot));
    h->size = size;
#ifdef DEBUG
    h->body = xcalloc(1, size);
#else
    h->body = xmalloc(size);
#endif
#ifdef __clang__ // XXX: ???
    memset(h->body, 0, MIN(size, sizeof(Header)));
#endif
    init_chunk(HEADER(h->body), size);
    HEADER_NEXT(HEADER(h->body)) = NULL;
    return h;
}

void gc_init(uintptr_t *volatile sp)
{
    stack_base = sp;
    init_size = align(init_size);
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
    if (curr->size >= size + sizeof(SchObject)/* == size of chunk */) {
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

#ifdef DEBUG
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
#ifdef DEBUG
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
    return v % 8U == 0 && v >= 0x100000;
}

static bool is_valid_header(Value v)
{
    Header *h = HEADER(v);
    return is_valid_tag(h->tag) &&
        h->size >= sizeof(SchObject) && h->size < sizeof(SchObject) * 2;
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

static bool is_markable(Value v)
{
    return is_valid_pointer(v) && is_valid_tag(HEADER(v)->tag);
}

static void mark_val(Value v)
{
    if (!is_markable(v))
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
    case TAG_ENV: {
        Env *p = ENV(v);
        table_foreach(p->table, mark_env_each, NULL);
        if (p->parent != Qfalse)
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

static void free_val(Value v)
{
    switch (VALUE_TAG(v)) {
    case TAG_CFUNC:
    case TAG_SYNTAX: {
        CFunc *p = CFUNC(v);
        free(p->name);
#ifdef DEBUG
        p->name = NULL;
        p->cfunc = NULL;
        p->proc = (Procedure) { 0, NULL };
#endif
        return;
    }
    case TAG_CONTINUATION: {
        Continuation *p = CONTINUATION(v);
        free((void *) p->exstate->stack);
        free(p->exstate);
#ifdef DEBUG
        p->exstate = NULL;
        p->proc = (Procedure) { 0, NULL };
#endif
        return;
    }
    case TAG_CLOSURE:
#ifdef DEBUG
        *PROCEDURE(v) = (Procedure) { 0, NULL };
#endif
        return;
    case TAG_HSTRING:
        free(HSTRING(v));
#ifdef DEBUG
        HSTRING(v) = NULL;
#endif
        return;
    case TAG_ENV: {
        Env *p = ENV(v);
        table_free(p->table);
        free(p->name);
#ifdef DEBUG
        p->name = NULL;
        p->table = NULL;
        p->parent = Qfalse;
#endif
        return;
    }
    case TAG_PORT: {
        FILE *fp = PORT(v)->fp;
        if (is_user_opened_file(fp))
            fclose(fp);
        PORT(v)->fp = NULL; // keep safety for fclose()
        return;
    }
    case TAG_VECTOR: {
        scary_free(VECTOR(v));
#ifdef DEBUG
        VECTOR(v) = NULL;
#endif
        return;
    }
    case TAG_ERROR: {
        scary_free(ERROR(v));
#ifdef DEBUG
        ERROR(v) = NULL;
#endif
        return;
    }
    case TAG_ESTRING:
    case TAG_PAIR:
    case TAG_EOF:
        return;
    case TAG_CHUNK:
        UNREACHABLE();
    }
}

static bool adjoining_p(const Header *prev, const Header *curr)
{
    if (prev == NULL || prev->tag != TAG_CHUNK)
        return false;
    void *prevnext = (uint8_t *) prev + prev->size;
    return prevnext == curr;
}

static void free_chunk(Header *prev, Header *curr)
{
    Value val = (Value) curr;
    free_val(val);
    if (adjoining_p(prev, curr)) {
        prev->size += curr->size;
#ifdef DEBUG
        memset(curr, 0, sizeof(SchObject));
#else
        curr->size = 0; // XXX: for is_valid_header ?
#endif
        return;
    }
    init_chunk(curr, curr->size);
    add_to_free_list(curr);
}

static void sweep_slot(HeapSlot *slot)
{
    uint8_t *p = slot->body, *endp = p + slot->size;
    size_t offset;
    for (Header *h, *prev = NULL; p < endp; p += offset, prev = h) {
        h = HEADER(p);
        offset = h->size;
        if (h->tag == TAG_CHUNK)
            continue;
        if (h->living) {
            h->living = false;
            continue;
        }
        free_chunk(prev, h);
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
    if (in_gc)
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

Header *gc_malloc(size_t size)
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
    if (p == NULL)
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
