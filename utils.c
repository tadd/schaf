#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

enum {
    SPACE_SIZE = 240*1024*1024,
};
uint8_t space[SPACE_SIZE];
ssize_t used;

void error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(2);
}

void *xmalloc(size_t size)
{
    size = (size + 7) / 8 * 8;
    if (used + size > SPACE_SIZE)
        error("malloc(%zu) failed", size);
    void *p = space + used;
    used += size;
    return p;
}

void *xcalloc(size_t nmem, size_t memsize)
{
    return xmalloc(nmem * memsize);
}
