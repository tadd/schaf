#ifndef UTILS_H
#define UTILS_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define ATTR(x) __attribute__((x))
#define UNREACHABLE() error("unreachable"), __builtin_unreachable()
#define ATTR_MALLOC ATTR(malloc) ATTR(used)
#define ATTR_XMALLOC ATTR_MALLOC ATTR(returns_nonnull)

ATTR(noreturn) ATTR(format(printf, 1, 2)) void error(const char *fmt, ...);
ATTR_XMALLOC void *xmalloc(size_t size);
ATTR_XMALLOC void *xrealloc(void *p, size_t size);
ATTR_XMALLOC char *xstrdup(const char *s);

typedef struct Table Table;
typedef bool (*TableEqualFunc)(uint64_t x, uint64_t y);
typedef uint64_t (*TableHashFunc)(uint64_t x);
typedef void (*TableFreeFunc)(void *p);
Table *table_new(void);
Table *table_new_str(void);
Table *table_new_full(TableHashFunc hash, TableEqualFunc eq, TableFreeFunc free_key);
void table_free(Table *t);
Table *table_put(Table *t, uint64_t key, uint64_t val); // `val` can't be 0
uint64_t table_get(const Table *t, uint64_t key);
bool table_set_or_put(Table *t, uint64_t key, uint64_t val);
Table *table_merge(Table *dst, const Table *src);

#define debug(fmt, ...) fprintf(stderr, fmt "\n" __VA_OPT__(,) __VA_ARGS__);

#endif
