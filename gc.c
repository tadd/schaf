#ifdef DEBUG
#include <assert.h>
#endif
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "intern.h"
#include "utils.h"

#if 1
#include <valgrind/memcheck.h>
#else
#define VALGRIND_MAKE_MEM_DEFINED(p, n) //
#endif

typedef struct {
    size_t size;
    uint8_t *body;
} Heap;

enum {
    TABMAX = 1024,
};

typedef struct {
    size_t size, used;
    size_t tab_free[TABMAX+1], tab_used[TABMAX+1];
} HeapStat;

typedef struct Chunk {
    size_t size; // size of managed space w/o header
    bool allocated;
    struct Chunk *next;
} Chunk;

#define CHUNK(v) ((Chunk *) (v))

enum {
    MiB = 1024 * 1024,
    HEAP_RATIO = 2,
    ROOT_SIZE = 0x08,
};

static size_t init_size = 1 * MiB;
// 64 is enough large, it can use up the entire 64-bit memory space
// (= (fold + 0 (map (cut expt 2 <>) (iota 64))) (- (expt 2 64) 1)) ;;=> #t
static Heap *heaps[64];
static size_t heaps_length;
static uint8_t *heaps_low, *heaps_high;

static Chunk *free_list;

static uintptr_t *stack_base;
static const Value *root[ROOT_SIZE];
static size_t nroot;
static Value user_objects;

static bool stress, print_stat;

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

#ifdef DEBUG
#define assert_ptr(p) do { \
        assert(p != NULL); \
        assert((uintptr_t) p % 8U == 0); \
        assert((uintptr_t) p > 0x1000); \
    } while (0)
#define assert_stack_ptr(p) do { \
        assert_ptr(p); \
        assert((uintptr_t) p % 8U == 0); \
    } while (0)
#define assert_ptr_if_nonnull(p) if (p) assert_ptr(p)
#define assert_bool(b) assert(b == false || b == true)
#define assert_chunk(p) do { \
        Chunk *c = (void *) p; \
        assert_ptr(c); \
        assert_ptr_if_nonnull(c->next); \
        assert_bool(c->allocated); \
        assert(c->size > 0); \
        assert(c->size < 1000*1000*1000); \
    } while (0)
#define assert_freelist() do { \
        assert_ptr_if_nonnull(free_list); \
        for (Chunk *p = free_list; p != NULL; p = p->next) { \
            assert_chunk(p); \
            assert(!p->allocated); \
        } \
    } while (0)
#define assert_heaps() do { \
        for (size_t i = 0; i < heaps_length; i++) { \
            Heap *heap = heaps[i]; \
            size_t offset; \
            uint8_t *p = heap->body, *endp = p + heap->size; \
            for (Chunk *ch; p < endp; p += offset) { \
                assert_chunk(p); \
                ch = CHUNK(p); \
                offset = sizeof(Chunk) + ch->size; \
                assert(offset % 8U == 0); \
            } \
            assert(p == endp); \
        } \
    } while (0)
#define assert_whole() do { assert_freelist(); assert_heaps(); } while (0)
#else
#define assert_ptr(p)
#define assert_stack_ptr(p)
#define assert_ptr_if_nonnull(p)
#define assert_bool(b)
#define assert_chunk(p) do {} while (0)
#define assert_freelist()
#define assert_heaps()
#define assert_whole()
#endif

static Heap *heap_new(size_t size)
{
    Heap *h = xcalloc(1, sizeof(Heap));
    assert_ptr(h);
    h->size = size;
    h->body = xcalloc(1, size);
    assert_ptr(h->body);
    Chunk *ch = (Chunk *) h->body;
    ch->size = size - sizeof(Chunk);
    ch->allocated = false;
    return h;
}

void gc_fin(void)
{
    for (size_t i = 0; i < heaps_length; i++) {
        free(heaps[i]->body);
        free(heaps[i]);
    }
}

void gc_init(uintptr_t *sp)
{
    stack_base = sp;
    init_size = align(init_size);
    heaps_length = 1;
    heaps[0] = heap_new(init_size);
    heaps_low = heaps[0]->body;
    heaps_high = heaps_low + heaps[0]->size;

    Chunk *ch = (Chunk *) heaps_low;
    assert_ptr(ch);
    ch->next = NULL;
    free_list = ch;

    user_objects = Qnil;
    gc_add_root(&user_objects);

    assert_whole();
}

// Allocation

static void *allocate_from_chunk(Chunk *prev, Chunk *curr, size_t size)
{
    assert_freelist();
    assert_chunk(curr);
    if (prev)
        assert_chunk(prev);
    size_t hsize = sizeof(Chunk) + size;
    Chunk *next = curr->next;
    if (next)
        assert_chunk(next);
    if (curr->size > hsize) {
        assert_chunk(curr);
        Chunk *rest = (Chunk *)((uint8_t *) curr + hsize);
        rest->size = curr->size - hsize;
        rest->allocated = false;
        rest->next = next;
        assert_chunk(rest);
        next = rest;
        curr->size = size;
    }
    if (prev == NULL)
        free_list = next;
    else
        prev->next = next;
    curr->size = size;
    curr->allocated = true;
    assert_chunk(curr);
    return curr + 1;
}

static void *allocate(size_t size)
{
    assert_freelist();
    size = align(size);
    size_t hsize = sizeof(Chunk) + size;
    for (Chunk *prev = NULL, *curr = free_list; curr != NULL; prev = curr, curr = curr->next) {
        assert_ptr(curr);
        if (curr->size >= hsize) // First-fit
            return allocate_from_chunk(prev, curr, size);
    }
    assert_freelist();
    return NULL;
}

size_t gc_stack_get_size(uintptr_t *sp)
{
    return (uint8_t *) stack_base - (uint8_t *) sp;
}

// Marking

static void mark_val(Value v);

void sch_gc_mark(Value v)
{
    mark_val(v);
}

static Value user_obj_new(const char *typename, GCFunction mark, GCFunction ffree, void *p)
{
    UserObject *o = obj_new(sizeof(UserObject), TAG_USER_OBJ);
    o->name = xmalloc(strlen(typename) + 1);
    strcpy(o->name, typename);
    o->obj = p;
    o->mark = mark;
    o->free = ffree;
    return (Value) o;
}

void sch_register_user_obj(const char *typename, GCFunction mark, GCFunction ffree, void *p)
{
    Value v = user_obj_new(typename, mark, ffree, p);
    user_objects = cons(v, user_objects);
}

static void mark_env_each(uint64_t key, uint64_t val, ATTR(unused) void *data)
{
    mark_val(key);
    mark_val(val);
}

static void mark_env(Table *env)
{
    table_foreach(env, mark_env_each, NULL);
}

bool in_heap_range(uintptr_t v)
{
    const uint8_t *p = (uint8_t *) v;
    if (p == NULL || p < heaps_low || p > heaps_high)
        return false;
    for (size_t i = 0; i < heaps_length; i++) {
        Heap *heap = heaps[i];
        if (p >= heap->body &&
            p < heap->body + heap->size &&
            (p - heap->body) % sizeof(uintptr_t) == 0)
            return true;
    }
    return false;
}

static bool in_heap_val(Value v)
{
    if (value_is_immediate(v) ||
        !in_heap_range(v))
        return false;
    ValueTag t = VALUE_TAG(v);
    return t >= TAG_PAIR && t <= TAG_LAST; // need to be precise more?
}

static void mark_array(void *beg, size_t n)
{
    assert_stack_ptr(beg);
    VALGRIND_MAKE_MEM_DEFINED(beg, n * sizeof(uintptr_t));
    Value *p = beg;
    for (size_t i = 0; i < n; i++, p++) {
        if (in_heap_val(*p))
            mark_val(*p);
    }
}

static void mark_jmpbuf(jmp_buf *jmp)
{
    mark_array(&jmp, (size_t) sizeof(jmp_buf) / sizeof(uintptr_t));
}

static void mark_val(Value v)
{
    if (value_is_immediate(v))
        return;
    Header *h = HEADER(v);
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
        mark_jmpbuf(&p->state);
        mark_array(p->stack, p->stack_len / sizeof(uintptr_t));
        return;
    }
    case TAG_USER_OBJ: {
        UserObject *p = USER_OBJ(v);
        if (p->mark != NULL)
            p->mark(p->obj);
        return;
    }
    case TAG_STRING:
    case TAG_CFUNC:
    case TAG_SYNTAX:
        return;
    }
}

void gc_add_root(const Value *r)
{
    if (nroot == ROOT_SIZE)
        error("%s: too many roots added", __func__);
    root[nroot++] = r;
}

static void mark_roots(void)
{
    for (size_t i = 0; i < nroot; i++)
        mark_val(*root[i]);
}

#define GET_SP(p) uintptr_t v##p = 0, *p = &v##p

ATTR(noinline)
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

ATTR(unused)
static bool chunk_header_equal(Chunk *a, Chunk *b)
{
    return a->size == b->size &&
        a->allocated == b->allocated;
}

ATTR(unused)
static void heap_dump_single(const Heap *heap)
{
    uint8_t *p = heap->body, *endp = p + init_size;
    fprintf(stderr, "begin: %p..%p\n", p, endp);
    size_t offset;
    bool ellipsis = false;
    for (Chunk *ch, *prev = NULL; p < endp; p += offset, prev = ch) {
        ch = CHUNK(p);
        offset = sizeof(Chunk) + ch->size;
        if (prev != NULL && chunk_header_equal(ch, prev)) {
            if (!ellipsis) {
                fprintf(stderr, "  [..]\n");
                ellipsis = true;
            }
            continue;
        }
        ellipsis = false;
        fprintf(stderr, "  [%p] size: %2zu, alloc: %d\n",
                ch, ch->size, ch->allocated);
    }
    fprintf(stderr, "end: %p..%p\n", p, endp);
}

ATTR(unused)
static void heap_dump(void)
{
    for (size_t i = 0; i < heaps_length; i++)
        heap_dump_single(heaps[i]);
}

static void heap_stat(HeapStat *stat)
{
    stat->size = stat->used = 0;
    memset(stat->tab_free, 0, sizeof(stat->tab_free));
    memset(stat->tab_used, 0, sizeof(stat->tab_used));
    for (size_t i = 0; i < heaps_length; i++) {
        Heap *heap = heaps[i];
        stat->size += heap->size;
        size_t offset;
        for (uint8_t *p = heap->body, *endp = p + heap->size; p < endp; p += offset) {
            Chunk *ch = CHUNK(p);
            offset = sizeof(Chunk) + ch->size;
            size_t j = ch->size - 1;
            if (j > TABMAX)
                j = TABMAX;
            if (ch->allocated) {
                stat->used += offset;
                stat->tab_used[j]++;
            } else
                stat->tab_free[j]++;
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

static void add_to_free_list(Chunk *ch)
{
    assert_freelist();
    assert_chunk(ch);
    ch->next = free_list; // prepend
    free_list = ch;
    assert_chunk(ch);
    assert_freelist();
}

static bool user_obj_equal(UserObject *x, UserObject *y)
{
    return x->obj == y->obj &&
        x->mark == y->mark &&
        x->free == y->free &&
        strcmp(x->name, y->name) == 0;
}

static void unregister_user_obj(UserObject *o)
{
    for (Value curr = user_objects, prev = Qnil; curr != Qnil; prev = curr, curr = cdr(curr)) {
        if (!user_obj_equal(USER_OBJ(curr), o))
            continue;
        Value next = cdr(curr);
        if (prev == Qnil)
            user_objects = next;
        else
            PAIR(prev)->cdr = next;
        return;
    }
    // UNREACHABLE();
}

static void free_val(Value v)
{
    return; // FIXME!
    if (value_is_immediate(v))
        return;
    switch (VALUE_TAG(v)) {
    case TAG_CLOSURE:
        env_free(CLOSURE(v)->env);
        return;
    case TAG_CONTINUATION:
        free(CONTINUATION(v)->stack);
        return;
    case TAG_USER_OBJ: {
        UserObject *p = USER_OBJ(v);
        unregister_user_obj(p);
        free(p->name);
        if (p->free != NULL)
            p->free(p->obj);
        return;
    }
    case TAG_STRING:
        free(STRING(v)->body);
        return;
    case TAG_PAIR:
    case TAG_CFUNC:
    case TAG_SYNTAX:
        return;
    }
}

static bool adjoining_p(const Chunk *prev, const Chunk *curr)
{
    return false; // FIXME!
    if (prev == NULL)
        return false;
    void *prevnext = (uint8_t *) (prev + 1) + prev->size;
    return prevnext == curr;
}

static Chunk *free_chunk(Chunk *prev, Chunk *curr)
{
    Value val = (Value) (curr + 1);
    if (in_heap_val(val))
        free_val(val);
    if (adjoining_p(prev, curr)) {
        debug("here");
        size_t hsize = sizeof(Chunk) + curr->size;
        // memset(curr, 0, hsize);
        prev->size += hsize;
        return prev;
    }
    // memset(curr + 1, 0, curr->size);
    curr->allocated = false;
    add_to_free_list(curr);
    return curr;
}

static void sweep_heap(Heap *heap)
{
    uint8_t *p = heap->body, *endp = p + heap->size;
    size_t offset;
    for (Chunk *ch, *prev = NULL; p < endp; p += offset, prev = ch) {
        assert_freelist();
        ch = CHUNK(p);
        assert_chunk(ch);
        offset = sizeof(Chunk) + ch->size;
        if (!ch->allocated)
            continue;
        Value v = (Value) (ch + 1);
        Header *h = HEADER(v);
        if (h->living) {
            h->living = false;
            continue;
        }
        ch = free_chunk(prev, ch);
        offset = sizeof(Chunk) + ch->size;
    }
}

static void sweep(void)
{
    assert_heaps();
    for (size_t i = 0; i < heaps_length; i++)
        sweep_heap(heaps[i]);
    assert_heaps();
}

static void add_heap(void)
{
    Heap *last = heaps[heaps_length-1];
    size_t new_size = last->size * HEAP_RATIO;
    last = heaps[heaps_length++] = heap_new(new_size);
    uint8_t *beg = last->body, *end = beg + last->size;
    if (heaps_low > beg)
        heaps_low = beg;
    if (heaps_high < end)
        heaps_high = end;
    Chunk *ch = (Chunk *) last->body;
    ch->next = free_list;
    free_list = ch;
    assert_chunk(ch);
    assert_freelist();
}

ATTR(unused)
static size_t heap_used(void)
{
    HeapStat stat;
    heap_stat(&stat);
    return stat.used;
}

// Entry: gc() and gc_malloc()

static void gc(void)
{
    assert_freelist();
    assert_heaps();
    if (print_stat)
        heap_print_stat("GC begin");
#ifdef DEBUG
    size_t before_used = heap_used();
#endif
    // debug("GC: mark");
    mark();
    assert_freelist();
    assert_heaps();
    // debug("GC: sweep");
    sweep();
#ifdef DEBUG
    assert(heap_used() <= before_used);
#endif
    assert_freelist();
    assert_heaps();
    // debug("GC: maybe increase");
    if (print_stat)
        heap_print_stat("GC end");
    assert_freelist();
    assert_heaps();
}

static size_t heap_size(void)
{
    HeapStat stat;
    heap_stat(&stat);
    return stat.size;
}

void *gc_malloc(size_t size)
{
    assert_freelist();
    assert_heaps();
    if (stress)
        gc();
    size = align(size);
    void *p = allocate(size);
    if (!stress && p == NULL) {
        gc();
        p = allocate(size);
    }
    if (p == NULL) {
        add_heap();
        p = allocate(size);
    }
    if (p == NULL)
        error("unreachable: heap (~%zu MiB) exhausted", heap_size() / MiB);
    assert_ptr(p);
    assert_freelist();
    assert_heaps();
    memset(p, 0, size); // for debug
    assert_freelist();
    assert_heaps();
    return p;
}
