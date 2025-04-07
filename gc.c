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
    MiB = 1024 * 1024,
    HEAP_RATIO = 2,
};

static size_t init_size = 1 * MiB;
// 64 is enough large, it can use up the entire 64-bit memory space
static Heap *heaps[64];
static size_t heaps_length;
static const volatile uint8_t *stack_base;

static bool stress;

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
    return h;
}

void gc_fin(void)
{
    for (size_t i = 0; i < heaps_length; i++) {
        free(heaps[i]->body);
        free(heaps[i]);
    }
}

void gc_init(void)
{
    init_size = align(init_size);
    heaps[0] = heap_new(init_size);
    heaps_length = 1;
}

static void *allocate(size_t size)
{
    size = align(size);
    Heap *heap = heaps[heaps_length-1]; // use the last heap only
    if (heap->used + size > heap->size)
        return NULL;
    uint8_t *ret = heap->body + heap->used;
    heap->used += size;
    return ret;
}

void gc_stack_init(const volatile void *b)
{
    stack_base = b;
}

size_t gc_stack_get_size(const volatile void *sp)
{
    return stack_base - (uint8_t *) sp;
}

static bool enough_free_space(void)
{
    static const size_t minreq = sizeof(Continuation); // maybe the largest
    Heap *heap = heaps[heaps_length-1];
    return (heap->size - heap->used) >= minreq;
}

static void increase_heaps(void)
{
    Heap *heap = heaps[heaps_length-1];
    heaps[heaps_length++] = heap_new(heap->size * HEAP_RATIO);
}

static void gc(void)
{
    // collects nothing
    if (!enough_free_space())
        increase_heaps();
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
    void *p = allocate(size);
    if (p == NULL) {
        gc();
        p = allocate(size);
    }
    if (p == NULL)
        error("out of memory; heap (~%lld MiB) exhausted",
              llround(heaps_size() / MiB));
    return p;
}
