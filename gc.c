#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "gc.h"
#include "utils.h"

#define HEADER(v) ((Header *) v)

typedef struct {
    size_t size;
    bool allocated;
    bool living;
} Header;

typedef struct Chunk {
    Header h;
    struct Chunk *next;
} Chunk;

enum {
    ROOT_SIZE = 0x10
};
static const size_t INIT_SIZE = 25 * (1024 * 1024); // MiB
static void *heap;
static Chunk free_list[1];
static const Value *root[ROOT_SIZE];
static long nroot;

void gc_init(void)
{
    heap = malloc(INIT_SIZE);
    if (heap == NULL)
        error("out of memory; initial malloc(%zu) failed", INIT_SIZE);
    Chunk *ch = heap;
    ch->h.size = INIT_SIZE - sizeof(Header);
    ch->h.allocated = false;
    ch->h.living = false;
    ch->next = NULL;
    free_list->next = ch; // free_list itself is never used as a chunk
}

static void *allocate_from_list(Chunk *prev, Chunk *curr, size_t size)
{
    uint8_t *p = (uint8_t *) curr;
    size_t hsize = size + sizeof(Header);
    if (curr->h.size == hsize)
        prev->next = curr->next;
    else {
        Header h = curr->h;
        h.size -= hsize;
        Chunk *next = curr->next;
        Chunk *ch = (Chunk *)(p + hsize);
        ch->h = h;
        ch->next = next;
        prev->next = ch;
    }
    Header *o = HEADER(p);
    o->size = size;
    o->allocated = true;
    return o + 1; // user of allocation use curr->next space and so-on
}

static void *allocate(size_t size)
{
    size_t hsize = size + sizeof(Header);
    for (Chunk *prev = free_list, *curr; (curr = prev->next) != NULL; prev = curr) {
        if (curr->h.size >= hsize) // First-fit
            return allocate_from_list(prev, curr, size);
    }
    return NULL;
}

void gc_add_root(const Value *r)
{
    if (nroot == ROOT_SIZE)
        error("%s: too many roots added", __func__);
    root[nroot++] = r;
}

static inline Header *get_header(Value v)
{
    return HEADER(v) - 1;
}

static void mark(Value v)
{
    if (value_is_immediate(v) || v == Qnil)
        return;
    Header *h = get_header(v);
    if (h->living)
        return;
    h->living = true;
    switch (VALUE_TAG(v)) {
    case TAG_PAIR: {
        Pair *p = PAIR(v);
        mark(p->car);
        mark(p->cdr);
        return;
    }
    case TAG_CLOSURE: {
        Closure *p = CLOSURE(v);
        mark(p->env);
        mark(p->params);
        mark(p->body);
        return;
    }
    case TAG_CONTINUATION: {
        Continuation *p = CONTINUATION(v);
        mark(p->call_stack);
        mark(p->retval);
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
    for (size_t i = 0; i < ROOT_SIZE; i++) {
        if (root[i] != NULL)
            mark(*root[i]);
    }
}

ATTR(unused)
static void heap_dump(void)
{
    uint8_t *p = heap;
    uint8_t *endp = p + INIT_SIZE;
    fprintf(stderr, "begin: %p..%p\n", p, endp);
    bool ellipsis = false;
    for (Header *h, *prev = NULL; p < endp; p += h->size + sizeof(Header), prev = h) {
        h = HEADER(p);
        if (prev != NULL && h->size == prev->size &&
            h->allocated == prev->allocated && h->living == prev->living) {
            if (!ellipsis) {
                fprintf(stderr, "[..]\n");
                ellipsis = true;
            }
            continue;
        }
        ellipsis = false;
        fprintf(stderr, "[%p] size: %zu, alloc: %d, liv: %d\n",
                h, h->size, h->allocated, h->living);
    }
    fprintf(stderr, "end: %p..%p\n", p, endp);
}

#define TABMAX 1024

ATTR(unused)
static void heap_stat_table(size_t tab[])
{
    for (size_t i = 0; i < TABMAX; i++) {
        if (tab[i] > 0)
            fprintf(stderr, "  [%zu] %zu\n", i, tab[i]);
    }
}

ATTR(unused)
static void heap_stat(void)
{
    uint8_t *p = heap;
    uint8_t *endp = p + INIT_SIZE;
    size_t used = 0;
    size_t tab_used[TABMAX+1] = { 0, }, tab_free[TABMAX+1] = { 0, };
    for (Header *h; p < endp; p += h->size + sizeof(Header)) {
        h = HEADER(p);
        size_t i = h->size > TABMAX ? TABMAX : h->size;
        if (h->allocated) {
            used += sizeof(Header) + h->size;
            tab_used[i]++;
        } else
            tab_free[i]++;
    }
    int n = ceil(log10(INIT_SIZE));
    long r = lround(((double) used / INIT_SIZE) * 1000);
    debug("heap usage: %*zu / %*zu (%3ld.%1ld%%)",
          n, used, n, INIT_SIZE, r/10, r%10);
    debug("used dist:");
    heap_stat_table(tab_used);
    debug("free dist:");
    heap_stat_table(tab_free);
}

static void sweep(void)
{
    uint8_t *p = heap;
    uint8_t *endp = p + INIT_SIZE;
    for (Header *h; p < endp; p += h->size + sizeof(Header)) {
        h = HEADER(p);
        if (h->allocated && !h->living)
            xfree(h);
        h->living = false;
    }
}

static void gc(void)
{
    //fprintf(stderr, "gc begin\n");
    //heap_stat();
    mark_roots();
    sweep();
    //heap_stat();
    //fprintf(stderr, "gc end\n");
}

void *xmalloc(size_t size)
{
    void *p = allocate(size);
    if (p == NULL) {
        gc();
        p = allocate(size);
    }
    if (p == NULL)
        error("out of memory; %s(%zu) failed", __func__, size);
    return p;
}

void *xrealloc(void *q, size_t size)
{
    void *p = allocate(size);
    if (p == NULL)
        error("out of memory; %s(ptr, %zu) failed", __func__, size);
    Header *op = p, *oq = q;
    memcpy(p, q, oq->size);
    xfree(q);
    op->size = size;
    return p;
}

void xfree(void *p)
{
    Chunk *ch = p;
    ch->h.allocated = false;
    ch->next = free_list->next; // prepend
    free_list->next = ch;
}
