#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "intern.h"
#include "utils.h"

enum {
    MiB = 1024 * 1024,
};
static size_t init_size = 1 * MiB;
static size_t heap_size, heap_used;
static uint8_t *heap;
static const volatile uint8_t *stack_base;

void sch_set_gc_init_size(size_t init_mib)
{
    init_size = init_mib * MiB;
}

void gc_init(void)
{
    heap = malloc(init_size);
    if (heap == NULL)
        error("out of memory; initial malloc(%zu) failed", init_size);
    heap_size = init_size;
    heap_used = 0;
}

static inline size_t align(size_t size)
{
    return (size + 7U) / 8U * 8U;
}

static void *allocate(size_t size)
{
    size = align(size);
    if (heap_used + size > heap_size)
        error("out of memory; heap (%zu MiB) exhausted", heap_size / MiB);
    uint8_t *ret = heap + heap_used;
    heap_used += size;
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

#define GET_SP(p) volatile void *p = &p

static void gc(void)
{
    // do the Epsilon
}

void *gc_malloc(size_t size)
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
