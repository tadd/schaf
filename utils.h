#ifndef UTILS_H
#define UTILS_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __SANITIZE_ADDRESS__
#include <sanitizer/asan_interface.h>
#define UNPOISON(p, n) ASAN_UNPOISON_MEMORY_REGION(p, n)
#else
#define UNPOISON(p, n) //nothing
#endif

#define UNUSED [[maybe_unused]]
#define UNREACHABLE() \
    error("%s:%d: %s: unreachable", __FILE__, __LINE__, __func__), \
    __builtin_unreachable()
#define ATTR_XMALLOC [[nodiscard]] [[gnu::malloc]] [[gnu::returns_nonnull]]

[[gnu::noreturn]] [[gnu::format(printf, 1, 2)]] void error(const char *fmt, ...);
ATTR_XMALLOC void *xmalloc(size_t size);
ATTR_XMALLOC void *xcalloc(size_t nmem, size_t memsize);
ATTR_XMALLOC char *xstrdup(const char *s);

#define debug(fmt, ...) fprintf(stderr, fmt "\n" __VA_OPT__(,) __VA_ARGS__);

typedef struct Table Table;
typedef void (*TableForeachFunc)(uint64_t key, uint64_t val, void *data);

extern const uint64_t TABLE_NOT_FOUND;
Table *table_new(void);
Table *table_inherit(const Table *t);
void table_free(Table *t);
Table *table_put(Table *t, uint64_t key, uint64_t val); // `val` can't be TABLE_NOT_FOUND
bool table_set(Table *t, uint64_t key, uint64_t val); // set if found
uint64_t table_get(const Table *t, uint64_t key);
void table_foreach(const Table *t, TableForeachFunc f, void *data);
UNUSED void table_dump(const Table *t); // for debug

#endif
