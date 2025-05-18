#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "intern.h"
#include "utils.h"

enum {
    MiB = 1024 * 1024,
    HEAP_RATIO = 2,
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

static size_t init_size = 1 * MiB;
static Heap heap;

static uintptr_t *stack_base;

static bool stress;

void sch_set_gc_init_size(double init_mib)
{
    init_size = round(init_mib * MiB);
}

void sch_set_gc_stress(bool b)
{
    stress = b;
}

static inline size_t align(size_t size)
{
    return (size + 7U) / 8U * 8U;
}

static HeapSlot *heap_slot_new(size_t size)
{
    HeapSlot *h = xcalloc(1, sizeof(HeapSlot));
    h->size = size;
    h->used = 0;
    h->body = xcalloc(1, size);
    return h;
}

void gc_fin(void)
{
    for (size_t i = 0; i < heap.size; i++) {
        free(heap.slot[i]->body);
        free(heap.slot[i]);
    }
}

void gc_init(uintptr_t *sp)
{
    stack_base = sp;
    init_size = align(init_size);
    heap.slot[0] = heap_slot_new(init_size);
    heap.size = 1;
}

static void *allocate(size_t size)
{
    HeapSlot *last = heap.slot[heap.size-1]; // use the last slot only
    if (last->used + size > last->size)
        return NULL;
    uint8_t *ret = last->body + last->used;
    last->used += size;
    return ret;
}

size_t gc_stack_get_size(uintptr_t *sp)
{
    return (uint8_t *) stack_base - (uint8_t *) sp;
}

static bool enough_free_space(void)
{
    static const size_t minreq = sizeof(Continuation); // maybe the largest
    HeapSlot *last = heap.slot[heap.size-1];
    return (last->size - last->used) >= minreq;
}

static void increase_heaps(void)
{
    HeapSlot *last = heap.slot[heap.size-1];
    heap.slot[heap.size++] = heap_slot_new(last->size * HEAP_RATIO);
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
    return init_size * (pow(HEAP_RATIO, heap.size) - 1)
        / (HEAP_RATIO - 1);
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
        error("out of memory; heap (~%lld MiB) exhausted",
              llround(heaps_size() / MiB));
    return p;
}
