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

static const size_t INIT_SIZE = 25 * (1024 * 1024); // MiB
static void *heap;
static Chunk free_list[1];

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
    memset(o + 1, 0, size);
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

void *xmalloc(size_t size)
{
    void *p = allocate(size);
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
