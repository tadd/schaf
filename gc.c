#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "intern.h"
#include "utils.h"

typedef struct {
    size_t size, used;
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
    bool living;
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
static Heap *heaps[64];
static size_t heaps_length;

static Chunk *free_list;

static const volatile uint8_t *stack_base;
static const Value *root[ROOT_SIZE];
static size_t nroot;
static Table **topenv;
static Value user_objects;
static Table *envs; // used in marking

static bool stress, print_stat;

void sch_set_gc_init_size(size_t init_mib)
{
    init_size = init_mib * MiB;
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

static Heap *heap_new(size_t size)
{
    Heap *h = xcalloc(1, sizeof(Heap));
    h->size = size;
    h->used = 0;
    h->body = xcalloc(1, size);
    return h;
}

void gc_fin(void)
{
    for (size_t i = 0; i < heaps_length; i++) {
        free(heaps[i]->body);
        free(heaps[i]);
    }
}

void gc_init(volatile void *sp)
{
    stack_base = sp;
    init_size = align(init_size);
    heaps[0] = heap_new(init_size);
    heaps_length = 1;

    Chunk *ch = (void *) heaps[0]->body;
    ch->size = init_size - sizeof(Chunk);
    ch->allocated = false;
    ch->living = false;
    ch->next = NULL;
    free_list = ch;
}

static void *allocate_from_chunk(Chunk *prev, Chunk *curr, size_t size)
{
    size_t hsize = sizeof(Chunk) + size;
    Chunk *next = curr->next;
    if (curr->size > hsize) {
        Chunk *rest = (Chunk *)((uint8_t *) curr + hsize);
        rest->size = curr->size - hsize;
        rest->allocated = rest->living = false;
        rest->next = next;
        next = rest;
        curr->size = size;
    }
    if (prev == NULL)
        free_list = next;
    else
        prev->next = next;
    curr->size = size;
    curr->allocated = true;
    return curr + 1;
}

static void *allocate(size_t size)
{
    size = align(size);
    size_t hsize = sizeof(Chunk) + size;
    for (Chunk *prev = NULL, *curr = free_list; curr != NULL; prev = curr, curr = curr->next) {
        if (curr->size >= hsize) // First-fit
            return allocate_from_chunk(prev, curr, size);
    }
    return NULL;
}

size_t gc_stack_get_size(const volatile void *sp)
{
    return stack_base - (uint8_t *) sp;
}

void sch_gc_mark(ATTR(unused) Value v)
{
    // collects nothing
}

static Value user_obj_new(const char *typename, GCFunction mark, GCFunction ffree, void *p)
{
    size_t len = strlen(typename) + 1;
    UserObject *o = obj_new(sizeof(UserObject) + len, TAG_USER_OBJ);
    o->mark = mark;
    o->free = ffree;
    o->obj = p;
    strcpy(o->name, typename);
    return (Value) o;
}

void sch_register_user_obj(const char *typename, GCFunction mark, GCFunction ffree, void *p)
{
    Value v = user_obj_new(typename, mark, ffree, p);
    user_objects = cons(v, user_objects);
}

static inline Chunk *get_chunk(Value v)
{
    return (Chunk *) v - 1;
}

static void mark_val(Value v);

static void mark_env_each(uint64_t key, uint64_t val, ATTR(unused) void *data)
{
    mark_val(key);
    mark_val(val);
}

static void mark_env(Table *env)
{
    uint64_t key = (uint64_t) env;
    if (table_get(envs, key) == TABLE_NOT_FOUND)
        table_put(envs, key, 1); // mark it
    table_foreach(env, mark_env_each, NULL);
}

static void mark_val(Value v)
{
    if (value_is_immediate(v))
        return;
    Chunk *h = get_chunk(v);
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

static void mark_roots(void)
{
    for (size_t i = 0; i < nroot; i++)
        mark_val(*root[i]);
}

bool in_heap_range(uintptr_t v)
{
    const uint8_t *p = (uint8_t *) v;
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
    return t >= TAG_PAIR && t <= TAG_SYNTAX; // need to be precise more?
}

static void mark_val_maybe(Value v)
{
    if (in_heap_val(v))
        mark_val(v);
}

static void mark_array(void *beg, size_t n)
{
    Value *p = beg;
    for (size_t i = 0; i < n; i++)
        mark_val_maybe(*p++);
}

#define GET_SP(p) uintptr_t tmp##p[1]; volatile void *p = tmp##p+1

ATTR(noinline)
static void mark_stack(void)
{
    GET_SP(sp);
    uintptr_t *beg = (uintptr_t *) sp, *end = (uintptr_t *) stack_base;
    mark_array(beg, end - beg);
}

ATTR(unused)
static bool header_equal(Chunk *a, Chunk *b)
{
    return a->size == b->size &&
        a->allocated == b->allocated &&
        a->living == b->living;
}

ATTR(unused)
static void heap_dump_single(const Heap *heap)
{
    uint8_t *p = heap->body, *endp = p + init_size;
    fprintf(stderr, "begin: %p..%p\n", p, endp);
    size_t offset;
    bool ellipsis = false;
    for (Chunk *h, *prev = NULL; p < endp; p += offset, prev = h) {
        h = CHUNK(p);
        offset = sizeof(Chunk) + h->size;
        if (prev != NULL && header_equal(h, prev)) {
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

void gc_add_root(const Value *r)
{
    if (nroot == ROOT_SIZE)
        error("%s: too many roots added", __func__);
    root[nroot++] = r;
}

void gc_add_root_env(Table **env)
{
    topenv = env;
}

static void heap_stat_table(size_t tab[])
{
    for (size_t i = 0; i < TABMAX; i++) {
        if (tab[i] > 0)
            fprintf(stderr, "    [%5zu] %zu\n", i+1, tab[i]);
    }
    if (tab[TABMAX] > 0)
        fprintf(stderr, "    [>%d] %zu\n", TABMAX, tab[TABMAX]);
}

static void heap_stat(HeapStat *stat)
{
    stat->size = stat->used = 0;
    memset(stat->tab_free, 0, sizeof(stat->tab_free));
    memset(stat->tab_used, 0, sizeof(stat->tab_used));
    for (size_t i = 0; i < heaps_length; i++) {
        Heap *heap = heaps[i];
        stat->size += heap->size;
        stat->used += heap->used;
        size_t offset;
        for (uint8_t *p = heap->body, *endp = p + heap->size; p < endp; p += offset) {
            Chunk *h = CHUNK(p);
            offset = sizeof(Chunk) + h->size;
            size_t j = h->size > TABMAX ? TABMAX : h->size-1;
            if (h->allocated) {
                stat->used += offset;
                stat->tab_used[j]++;
            } else
                stat->tab_free[j]++;
        }
    }
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
    heap_stat_table(stat.tab_used);
    debug("free:");
    heap_stat_table(stat.tab_free);
    debug("");
}

static void add_to_free_list(Chunk *h)
{
    Chunk *ch = (Chunk *) h;
    ch->next = free_list; // prepend
    free_list = ch;
}

static void free_env(Table *env)
{
    uint64_t key = (uint64_t) env;
    if (table_get(envs, key) == TABLE_NOT_FOUND)
        table_free(env);
}

static void free_val(Value v)
{
    if (value_is_immediate(v))
        return;
    switch (VALUE_TAG(v)) {
    case TAG_CLOSURE:
        free_env(CLOSURE(v)->env);
        return;
    case TAG_CONTINUATION:
        free(CONTINUATION(v)->shelter);
        return;
    case TAG_PAIR:
    case TAG_USER_OBJ: {
        UserObject *p = USER_OBJ(v);
        if (p->free != NULL)
            p->free(p->obj);
        return;
    }
    case TAG_STRING:
    case TAG_CFUNC:
    case TAG_SYNTAX:
        return;
    }
}

static Chunk *free_chunk(Chunk *prev, Chunk *curr, size_t offset)
{
    Value *val = (Value *) (curr + 1);
    if (in_heap_val(*val))
        free_val(*val);
    if (prev != NULL && !prev->allocated) {
        memset(curr, 0, offset);
        prev->size += offset;
        return prev;
    }
    memset(val, 0, curr->size);
    curr->allocated = false;
    add_to_free_list(curr);
    return curr;
}

static void sweep(void)
{
    // heap_dump();
    for (size_t i = 0; i < heaps_length; i++) {
        Heap *heap = heaps[i];
        uint8_t *p = heap->body, *endp = p + heap->size;
        size_t offset;
        for (Chunk *h, *prev = NULL; p < endp; p += offset) {
            h = CHUNK(p);
            offset = sizeof(Chunk) + h->size;
            if (!h->allocated)
                continue;
            if (h->living) {
                h->living = false;
                prev = h;
                continue;
            }
            prev = free_chunk(prev, h, offset);
        }
    }
    table_free(envs);
}

static void mark(void)
{
    envs = table_new();
    mark_stack();
    if (topenv)
        mark_env(*topenv);
    mark_roots();
    jmp_buf jmp;
    memset(&jmp, 0, sizeof(jmp_buf));
    setjmp(jmp);
    mark_array(&jmp, (size_t) sizeof(jmp_buf) / sizeof(uintptr_t));
}

static void increase_heaps(void)
{
    Heap *heap = heaps[heaps_length-1];
    heaps[heaps_length++] = heap_new(heap->size * HEAP_RATIO);
}

static bool enough_free_chunks(void)
{
    static const size_t minreq = sizeof(Continuation); // maybe the largest
    for (Chunk *curr = free_list; curr != NULL; curr = curr->next) {
        if (curr->size >= minreq)
            return true;
    }
    return false;
}

static void gc(void)
{
    if (print_stat)
        heap_print_stat("GC begin");
    mark();
    sweep();
    if (!enough_free_chunks())
        increase_heaps();
    if (print_stat)
        heap_print_stat("GC end");
}

static size_t heaps_size(void)
{
    HeapStat stat;
    heap_stat(&stat);
    return stat.size;
}

void *gc_malloc(size_t size)
{
    if (stress)
        gc();
    size = align(size);
    void *p = allocate(size);
    if (!stress && p == NULL) {
        gc();
        p = allocate(size);
    }
    if (p == NULL)
        error("out of memory; heap (~%zu MiB) exhausted",
              heaps_size() / MiB);
    memset(p, 0, size); // for debug
    return p;
}
