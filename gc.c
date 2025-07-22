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
    TABMAX = 1024,
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

static void *assert_48bits(void *p)
{
#if 0
    if ((uintptr_t) p > 0xFFFF'FFFF'FFFFUL) {
        debug("Pointer too large: %p", p);
        abort();
    }
#endif
    return p;
}

static HeapSlot *heap_slot_new(size_t size)
{
    HeapSlot *h = xcalloc(1, sizeof(HeapSlot));
    assert_48bits(h);
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
    return assert_48bits(ret);
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

static void heap_stat(HeapStat *stat)
{
    stat->size = stat->used = 0;
    memset(stat->tab_free, 0, sizeof(stat->tab_free));
    memset(stat->tab_used, 0, sizeof(stat->tab_used));
    for (size_t i = 0; i < heap.size; i++) {
        HeapSlot* slot = heap.slot[i];
        stat->size += slot->size;
        stat->used += slot->used;
    }
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
    return p;
}
