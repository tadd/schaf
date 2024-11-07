#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

void *xmalloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL)
        error("malloc(%zu) failed", size);
    return p;
}

void *xrealloc(void *q, size_t size)
{
    void *p = realloc(q, size);
    if (p == NULL)
        error("realloc(ptr, %zu) failed", size);
    return p;
}

void xfree(void *p)
{
    free(p); // as-is for now
}
