#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "intern.h"
#include "schaf.h"
#include "utils.h"

//
// General definitions
//

#ifdef DEBUG
#define xmalloc(size) xcalloc(1, size);
#endif

// Types
enum {
    MiB = 1024 * 1024,
    HEAP_RATIO = 2,
    TABMAX = 1024,
};

typedef struct {
    size_t size, used;
    size_t tab_free[TABMAX+1], tab_used[TABMAX+1];
} HeapStat;

typedef struct {
    void (*init)(const uintptr_t *volatile sp);
    void (*fin)(void);
    void *(*malloc)(size_t size);
    void (*add_root)(const Value *r);
    void (*stat)(HeapStat *stat);
} GCFunctions;

static void heap_print_stat(const char *header);
static size_t heap_size(void);
[[gnu::noreturn]] static void error_out_of_memory(void);

// Static data
static void *gc_data; // singleton; maybe a heap or some context
static GCFunctions funcs;
static size_t init_size = 1 * MiB;

static const uintptr_t *volatile stack_base;

static bool stress, print_stat;
static bool in_gc, initialized;

//
// Algorithm: Epsilon
//

typedef struct {
    size_t size, used;
    uint8_t *body;
} EpsHeapSlot;

typedef struct {
    size_t size;
    // 64 is enough large, it can use up the entire 64-bit memory space
    // (= (fold + 0 (map (cut expt 2 <>) (iota 64))) (- (expt 2 64) 1)) ;;=> #t
    EpsHeapSlot *slot[64];
} EpsHeap;

static inline size_t align(size_t size)
{
    return (size + 15U) / 16U * 16U;
}

static EpsHeapSlot *eps_heap_slot_new(size_t size)
{
    EpsHeapSlot *h = xmalloc(sizeof(EpsHeapSlot));
    h->size = align(size);
    h->used = 0;
    h->body = xmalloc(size);
    return h;
}

static void eps_init(const uintptr_t *volatile sp)
{
    stack_base = sp;
    EpsHeap *heap = xmalloc(sizeof(EpsHeap));
    heap->slot[0] = eps_heap_slot_new(init_size);
    heap->size = 1;
    gc_data = heap;
}

static void eps_fin(void)
{
    EpsHeap *heap = gc_data;
    for (size_t i = 0; i < heap->size; i++) {
        free(heap->slot[i]->body);
        free(heap->slot[i]);
    }
    free(heap);
}

static inline EpsHeapSlot *last_slot(EpsHeap *heap)
{
    return heap->slot[heap->size - 1];
}

static void *eps_allocate(size_t size)
{
    EpsHeap *heap = gc_data;
    EpsHeapSlot *last = last_slot(heap); // use the last slot only
    if (last->used + size > last->size)
        return NULL;
    uint8_t *ret = last->body + last->used;
    last->used += size;
    return ret;
}

static bool eps_has_minimum_free_space(void)
{
    EpsHeap *heap = gc_data;
    static const size_t minreq = sizeof(Continuation); // maybe the largest
    EpsHeapSlot *last = last_slot(heap);
    return (last->size - last->used) >= minreq;
}

static void eps_increase_heap(void)
{
    EpsHeap *heap = gc_data;
    EpsHeapSlot *last = last_slot(heap);
    heap->slot[heap->size++] = eps_heap_slot_new(last->size * HEAP_RATIO);
}

static void eps_add_root(UNUSED const Value *p) {} // dummy

static void eps_stat(HeapStat *stat)
{
    EpsHeap *heap = gc_data;
    memset(stat->tab_free, 0, sizeof(stat->tab_free));
    memset(stat->tab_used, 0, sizeof(stat->tab_used));
    stat->size = stat->used = 0;
    for (size_t i = 0; i < heap->size; i++) {
        EpsHeapSlot* slot = heap->slot[i];
        stat->size += slot->size;
        stat->used += slot->used;
    }
}

static void eps_gc(void)
{
    if (UNLIKELY(in_gc))
        bug("nested GC detected");
    in_gc = true;
    if (print_stat)
        heap_print_stat("GC begin");
    if (!eps_has_minimum_free_space())
        eps_increase_heap(); // collects nothing ;)
    if (print_stat)
        heap_print_stat("GC end");
    in_gc = false;
}

static void *eps_malloc(size_t size)
{
    if (stress)
        eps_gc();
    size = align(size);
    void *p = eps_allocate(size);
    if (!stress && p == NULL) {
        eps_gc();
        p = eps_allocate(size);
    }
    if (UNLIKELY(p == NULL))
        error_out_of_memory();
    return p;
}

static const GCFunctions GC_FUNCS_EPSILON = {
    eps_init, eps_fin, eps_malloc, eps_add_root, eps_stat
};
static const GCFunctions GC_FUNCS_DEFAULT = GC_FUNCS_EPSILON;

//
// General functions
//

static void error_out_of_memory(void)
{
    error("out of memory; heap (~%zu MiB) exhausted", heap_size() / MiB);
}

void gc_add_root(const Value *r)
{
    funcs.add_root(r);
}

static void heap_print_stat_table(const char *header, size_t tab[])
{
    debug("%s:", header);
    for (size_t i = 0; i < TABMAX; i++) {
        if (tab[i] > 0)
            fprintf(stderr, "    [%5zu] %zu\n", i+1, tab[i]);
    }
    if (tab[TABMAX] > 0)
        fprintf(stderr, "    [>%d] %zu\n", TABMAX, tab[TABMAX]);
}

static void heap_stat(HeapStat *stat)
{
    funcs.stat(stat);
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
    heap_print_stat_table("used", stat.tab_used);
    heap_print_stat_table("free", stat.tab_free);
    debug("");
}

static size_t heap_size(void)
{
    HeapStat stat;
    heap_stat(&stat);
    return stat.size;
}

#define error_if_gc_initialized() if (initialized) error("%s called after GC initialization", __func__)

void sch_set_gc_init_size(double init_mib)
{
    error_if_gc_initialized();
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

void sch_set_gc_algorithm(SchGCAlgorithm s)
{
    error_if_gc_initialized();
    switch (s) {
    case GC_ALGORITHM_EPSILON:
        funcs = GC_FUNCS_EPSILON;
        break;
    default:
        UNREACHABLE();
    }
}

void gc_init(const uintptr_t *volatile sp)
{
    if (funcs.init == NULL)
        funcs = GC_FUNCS_DEFAULT;
    funcs.init(sp);
    initialized = true;
}

void gc_fin(void)
{
    funcs.fin();
}

void *gc_malloc(size_t size)
{
    return funcs.malloc(size);
}

size_t gc_stack_get_size(const uintptr_t *volatile sp)
{
    return (uint8_t *volatile) stack_base - (uint8_t *volatile) sp;
}
