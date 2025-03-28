#include <assert.h>
#include <math.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "intern.h"
#include "utils.h"

enum {
    ROOT_SIZE = 0x08,
    MiB = 1024 * 1024,
    HEAP_RATIO = 2,
};
static size_t init_size = 1 * MiB;

typedef struct {
    size_t size, used;
    uint8_t *body;
} Heap;
// 64 is enough large, it can use up the entire 64-bit memory space
static Heap *heaps[64];
static size_t heaps_length;

#define HEADER(v) ((Header *) (v))
typedef struct {
    size_t size; // size of managed space w/o header
    bool allocated;
    bool living;
} Header;

typedef struct Chunk {
    Header h;
    alignas(sizeof(uintptr_t)) struct Chunk *next;
} Chunk;

enum {
    CHUNK_OFFSET = offsetof(Chunk, next),
};

static Chunk *free_list;

static bool stress, print_stat;
static const Value *root[ROOT_SIZE];
static size_t nroot;
static const volatile uint8_t *stack_base;

static Table **topenv;

void sch_set_gc_init_size(size_t init_mib)
{
    init_size = init_mib * MiB;
}

void sch_set_gc_stress(bool b)
{
    stress = b;
}

static inline size_t align(size_t size)
{
    return (size + 7U) / 8U * 8U;
}

static Heap *heap_new(size_t size)
{
    Heap *h = xmalloc(sizeof(Heap));
    h->size = size;
    h->used = 0;
    h->body = xmalloc(size);
    memset(h->body, 0, size);
    return h;
}

void gc_fin(void)
{
    for (size_t i = 0; i < heaps_length; i++) {
        free(heaps[i]->body);
        free(heaps[i]);
    }
}

#define assert_ptr(p) assert((uintptr_t) (p) > 0x1000)
#define assert_ptr_if_nonnull(p) if (p) assert_ptr(p)
#define assert_bool(b) assert(b == false || b == true)
#define assert_header(p) do { \
        Header *hd = HEADER(p); \
        assert_ptr(hd); \
        assert(hd->size); \
        assert_bool(hd->allocated); \
        assert_bool(hd->living); \
        assert(hd->size); \
        assert(hd->size < 1000*1000*1000); \
    } while (0)
#define assert_chunk(p) do { \
        Chunk *c = (void *) p; \
        assert_ptr(c); \
        assert_ptr_if_nonnull(c->next); \
        assert_header(c); \
    } while (0)
#define assert_freelist() do { \
        assert_ptr(free_list); \
        for (Chunk *p = free_list; p != NULL; p = p->next) { \
            assert_chunk(p); \
            assert(!p->h.allocated); \
        } \
    } while(0)
#define assert_heaps() do { \
        for (size_t i = 0; i < heaps_length; i++) { \
            Heap *heap = heaps[i]; \
            size_t offset; \
            uint8_t *p = heap->body, *endp = p + heap->size; \
            for (Header *h; p < endp; p += offset) { \
                assert_header(p); \
                h = HEADER(p); \
                offset = h->size + sizeof(Header); \
            } \
        } \
    } while (0)
#define assert_whole() do { assert_freelist(); assert_heaps(); } while (0)

void gc_init(void)
{
    init_size = align(init_size);
    heaps[0] = heap_new(init_size);
    heaps_length = 1;

    Chunk *ch = (void *) heaps[0]->body;
    assert_ptr(ch);
    ch->h.size = init_size - CHUNK_OFFSET;
    ch->h.allocated = false;
    ch->h.living = false;
    ch->next = NULL;
    free_list = ch;
    assert_whole();
}

static void *allocate_from_chunk(Chunk *prev, Chunk *curr, size_t size)
{
    assert_freelist();
    assert_chunk(curr);
    if (prev)
        assert_chunk(prev);
    size_t hsize = size + CHUNK_OFFSET;
    Chunk *next = curr->next;
    if (next)
        assert_chunk(next);
    if (curr->h.size > hsize) { 
        assert_chunk(curr);
        Header h = curr->h;
        h.size -= hsize;
        assert_header(&h);
        uint8_t *p = (uint8_t *) curr;
        Chunk *rest = (Chunk *)(p + hsize);
        rest->h = h;
        rest->next = next;
        assert_chunk(rest);
        next = rest;
    }
    assert_chunk(next);
    if (prev == NULL)
        free_list = next;
    else
        prev->next = next;
    assert_chunk(curr);
    Header *o = HEADER(curr);
    o->size = size;
    o->allocated = true;
    return &curr->next; // use curr->next space and so-on as allocated
}

static void *allocate(size_t size)
{
    assert_freelist();
    size = align(size);
    size_t hsize = size + CHUNK_OFFSET;
    for (Chunk *prev = NULL, *curr = free_list; curr != NULL; prev = curr, curr = curr->next) {
        assert_ptr(curr);
        if (curr->h.size >= hsize) // First-fit
            return allocate_from_chunk(prev, curr, size);
    }
    assert_freelist();
    return NULL;
}

void gc_stack_init(const volatile void *b)
{
    stack_base = b;
}

size_t gc_stack_get_size(const volatile void *sp)
{
    return stack_base - (uint8_t *) sp;
}

void gc_add_root(const Value *r)
{
    if (nroot == ROOT_SIZE)
        error("%s: too many roots added", __func__);
    root[nroot++] = r;
}

void gc_set_topenv(Table **env)
{
    topenv = env;
}

static inline Header *get_header(Value v)
{
    return HEADER((uint8_t *) v - CHUNK_OFFSET);
}

static void mark_val(Value v);

static void mark_env_each(uint64_t key, uint64_t val, ATTR(unused) void *data)
{
    mark_val(key);
    mark_val(val);
}

static void mark_env(Table *env)
{
    table_foreach(env, mark_env_each, NULL);
}

static void mark_val(Value v)
{
    if (value_is_immediate(v))
        return;
    Header *h = get_header(v);
    assert_header(h);
    if (h->living)
        return;
    h->living = true;
    switch (VALUE_TAG(v)) {
    case TAG_PAIR: {
        Pair *p = PAIR(v);
        mark_val(p->car);
        mark_val(p->cdr);
        return;
    }
    case TAG_CLOSURE: {
        Closure *p = CLOSURE(v);
        mark_env(p->env);
        mark_val(p->params);
        mark_val(p->body);
        return;
    }
    case TAG_CONTINUATION: {
        Continuation *p = CONTINUATION(v);
        mark_val(p->call_stack);
        mark_val(p->retval);
        return;
    }
    case TAG_STR:
    case TAG_CFUNC:
    case TAG_SYNTAX:
        return;
    }
}

static void mark_roots(void)
{
    for (size_t i = 0; i < nroot; i++)
        mark_val(*root[i]);
}

static bool in_heap_range(uintptr_t v)
{
    // if (v < sizeof(Header)) // cannot get Header*
    //     return false;
    const uint8_t *p = (uint8_t *) get_header(v);
    for (size_t i = 0; i < heaps_length; i++) {
        Heap *heap = heaps[i];
        if (p >= heap->body &&
            p < heap->body + heap->size)
            return true;
    }
    return false;
}

static bool in_heap(uintptr_t v)
{
    if (value_is_immediate(v) ||
        !in_heap_range(v))
        return false;
    ValueTag t = VALUE_TAG((Value) v);
    return t >= TAG_PAIR && t <= TAG_SYNTAX; // need to be precise more?
}

static void mark_val_maybe(uintptr_t v)
{
    if (in_heap(v)) {
        //debug("marking: %zx", v);
        mark_val((Value) v);
    }
}

static void mark_array(void *beg, void *end)
{
    for (uintptr_t *p = beg; p < (uintptr_t *) end; p++)
        mark_val_maybe(*p);
}

#define GET_SP(p) volatile void *p = &p

ATTR(noinline)
static void mark_stack(void)
{
    GET_SP(sp);
    uintptr_t *beg = (uintptr_t *) sp, *end = (uintptr_t *) stack_base;
    mark_array(beg, end);
}

ATTR(unused)
static bool header_equal(Header a, Header b)
{
    return a.size == b.size &&
        a.allocated == b.allocated &&
        a.living == b.living;
}

ATTR(unused)
static void heap_dump_single(const Heap *heap)
{
    uint8_t *p = heap->body, *endp = p + init_size;
    fprintf(stderr, "begin: %p..%p\n", p, endp);
    size_t offset;
    bool ellipsis = false;
    for (Header *h, *prev = NULL; p < endp; p += offset, prev = h) {
        h = HEADER(p);
        offset = h->size + CHUNK_OFFSET;
        if (prev != NULL && header_equal(*h, *prev)) {
            if (!ellipsis) {
                fprintf(stderr, "  [..]\n");
                ellipsis = true;
            }
            continue;
        }
        ellipsis = false;
        fprintf(stderr, "  [%p] size: %2zu, alloc: %d, liv: %d\n",
                h, h->size, h->allocated, h->living);
    }
    fprintf(stderr, "end: %p..%p\n", p, endp);
}

ATTR(unused)
static void heap_dump(void)
{
    for (size_t i = 0; i < heaps_length; i++)
        heap_dump_single(heaps[heaps_length-1]);
}

#define TABMAX 1024

static void heap_stat_table(size_t tab[])
{
    for (size_t i = 0; i <= TABMAX; i++) {
        if (tab[i] > 0)
            fprintf(stderr, "    [%zu] %zu\n", i, tab[i]);
    }
}

static void heap_stat(const char *header)
{
    if (header != NULL)
        debug("%s", header);
    size_t size = 0, used = 0;
    size_t tab_used[TABMAX+1] = { 0, }, tab_free[TABMAX+1] = { 0, };
    for (size_t i = 0; i < heaps_length; i++) {
        Heap *heap = heaps[i];
        uint8_t *p = heap->body, *endp = p + heap->size;
        size += heap->size;
        size_t offset;
        for (Header *h; p < endp; p += offset) {
            assert_header(p);
            h = HEADER(p);
            offset = h->size + CHUNK_OFFSET;
            size_t j = h->size > TABMAX ? TABMAX : h->size;
            if (h->allocated) {
                used += offset;
                tab_used[j]++;
            } else
                tab_free[j]++;
        }
    }
    int n = ceil(log10(size));
    long r = lround(((double) used / size) * 1000);
    debug("  heap usage: %*zu / %*zu (%3ld.%1ld%%)",
          n, used, n, size, r/10, r%10);
    debug("  used dist:");
    heap_stat_table(tab_used);
    debug("  free dist:");
    heap_stat_table(tab_free);
}

static void add_to_free_list(Header *h)
{
    assert_freelist();
    assert_header(h);
    Chunk *ch = (Chunk *) h;
    ch->next = free_list; // prepend
    assert_chunk(ch);
    free_list = ch;
    assert_freelist();
}

static void sweep(void)
{
    for (size_t i = 0; i < heaps_length; i++) {
        Heap *heap = heaps[i];
        uint8_t *p = heap->body, *endp = p + heap->size;
        size_t offset;
        for (Header *h, *prev = NULL; p < endp; p += offset) {
            assert_header(p);
            assert_ptr_if_nonnull(prev);
            h = HEADER(p);
            offset = h->size + CHUNK_OFFSET;
            if (!h->allocated)
                continue;
            if (h->living) {
                h->living = false;
                prev = h;
                continue;
            }
            if (prev != NULL && !prev->allocated)
                prev->size += offset;
            else {
                h->allocated = false;
                add_to_free_list(h);
                prev = h;
            }
        }
    }
}

void sch_set_gc_print_stat(bool b)
{
    print_stat = b;
}

static void mark(void)
{
    mark_roots();
    if (topenv)
        mark_env(*topenv);
    jmp_buf jmp;
    memset(&jmp, 0, sizeof(jmp_buf));
    setjmp(jmp);
    mark_array(&jmp, (uint8_t *) &jmp + sizeof(jmp_buf));
    mark_stack();
}

#define INC_HEAP 1
#if INC_HEAP
static void increase_heaps(void)
{
    Heap *heap = heaps[heaps_length-1];
    size_t newsize = heap->size * HEAP_RATIO;
    heaps[heaps_length++] = heap_new(newsize);
}

static bool enough_free_chunks(void)
{
    static const size_t minreq = sizeof(Continuation); // maybe the largest
    for (Chunk *curr = free_list; curr != NULL; curr = curr->next) {
        if (curr->h.size >= minreq)
            return true;
    }
    return false;
}
#endif

static void gc(void)
{
    if (stack_base == NULL)
        return; // before INIT_STACK(), too early
    assert_whole();
    if (print_stat)
        heap_stat("GC begin");
    mark();
    sweep();
#if INC_HEAP
    if (!enough_free_chunks())
        increase_heaps();
#endif
    if (print_stat)
        heap_stat("GC end");
    assert_whole();
}

static double heaps_size(void)
{
    // Sum of a geometric sequence
    return init_size * (pow(HEAP_RATIO, heaps_length) - 1)
        / (HEAP_RATIO - 1);
}

void *gc_malloc(size_t size)
{
    if (stress)
        gc();
    size = align(size);
    void *p = allocate(size);
    if (p == NULL) {
        gc();
        p = allocate(size);
    }
    if (p == NULL)
        error("out of memory; heap (~%lld MiB) exhausted",
              llround(heaps_size() / MiB));
    assert_ptr(p);
    memset(p, 0, size); // for debug
    return p;
}
