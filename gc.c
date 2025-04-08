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

enum {
    MiB = 1024 * 1024,
    HEAP_RATIO = 2,
    ROOT_SIZE = 0x08,
};

static size_t init_size = 1 * MiB;
// 64 is enough large, it can use up the entire 64-bit memory space
static Heap *heaps[64];
static size_t heaps_length;
static const volatile uint8_t *stack_base;
static const Value *root[ROOT_SIZE];
static size_t nroot;
static Table **topenv;

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

static void gc(void)
{
    if (print_stat)
        heap_print_stat("GC begin");
    // collects nothing
    if (!enough_free_space())
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
