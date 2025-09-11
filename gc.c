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
} HeapStat;

static size_t init_size = 1 * MiB;
static Heap heap; // singleton

static uintptr_t *volatile stack_base;

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
    return (size + 15U) / 16U * 16U;
}

static HeapSlot *heap_slot_new(size_t size)
{
    HeapSlot *h = xmalloc(sizeof(HeapSlot));
    h->size = size;
    h->used = 0;
#ifdef DEBUG
    h->body = xcalloc(1, size);
#else
    h->body = xmalloc(size);
#endif
    return h;
}

void gc_fin(void)
{
    for (size_t i = 0; i < heap.size; i++) {
        free(heap.slot[i]->body);
        free(heap.slot[i]);
    }
}

void gc_init(uintptr_t *volatile sp)
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

size_t gc_stack_get_size(uintptr_t *volatile sp)
{
    return (uint8_t *volatile) stack_base - (uint8_t *volatile) sp;
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

static void heap_stat(HeapStat *stat)
{
    stat->size = stat->used = 0;
    for (size_t i = 0; i < heap.size; i++) {
        HeapSlot* slot = heap.slot[i];
        stat->size += slot->size;
        stat->used += slot->used;
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
}

static void gc(void)
{
    if (UNLIKELY(in_gc))
        bug("nested GC detected");
    in_gc = true;
    if (print_stat)
        heap_print_stat("GC begin");
    // collects nothing
    if (!enough_free_space())
        increase_heaps();
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
    if (UNLIKELY(p == NULL))
        error("out of memory; heap (~%zu MiB) exhausted", heap_size() / MiB);
    return p;
}
