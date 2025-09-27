#ifndef UTILS_H
#define UTILS_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __SANITIZE_ADDRESS__
#include <sanitizer/asan_interface.h>
#else
#define ASAN_UNPOISON_MEMORY_REGION(p, n) //
#endif

#if defined(DEBUG) && defined(__linux__)
#include <valgrind/memcheck.h>
#else
#define VALGRIND_MAKE_MEM_DEFINED(p, n) //
#endif

#define UNPOISON(p, n) \
    VALGRIND_MAKE_MEM_DEFINED(p, n); ASAN_UNPOISON_MEMORY_REGION(p, n)

#define debug(fmt, ...) fprintf(stderr, fmt "\n" __VA_OPT__(,) __VA_ARGS__);
#define bug(fmt, ...) \
    error("[BUG] %s:%d: %s: " fmt, \
          __FILE__, __LINE__, __func__ __VA_OPT__(,) __VA_ARGS__)

#define LIKELY(x) __builtin_expect((x), 1)
#define UNLIKELY(x) __builtin_expect((x), 0)
#define UNUSED [[maybe_unused]]
#ifdef DEBUG
#define UNREACHABLE() bug("unreachable")
#else
#define UNREACHABLE() __builtin_unreachable()
#endif
#define ATTR_XMALLOC [[nodiscard, gnu::malloc, gnu::returns_nonnull]]

#pragma GCC visibility push(hidden) // also affects Clang

size_t idivceil(size_t n, size_t aligned);
size_t iceil(size_t n, size_t aligned);
[[gnu::noreturn, gnu::format(printf, 1, 2)]]
void error(const char *fmt, ...);
ATTR_XMALLOC void *xmalloc(size_t size);
ATTR_XMALLOC void *xcalloc(size_t nmem, size_t memsize);
ATTR_XMALLOC void *xrealloc(void *orig, size_t size);
ATTR_XMALLOC char *xstrdup(const char *s);
[[nodiscard, gnu::returns_nonnull]]
FILE *mopen(const char *s);
[[nodiscard, gnu::returns_nonnull]]
FILE *mopen_w(char **p);

typedef struct Table Table;
typedef void (*TableForeachFunc)(uint64_t key, uint64_t val);

extern const uint64_t TABLE_NOT_FOUND;
Table *table_new(void);
Table *table_dup(const Table *orig);
void table_free(Table *t);
Table *table_put(Table *t, uint64_t key, uint64_t val); // `val` can't be TABLE_NOT_FOUND
bool table_set(Table *t, uint64_t key, uint64_t val); // set if found
uint64_t table_get(const Table *t, uint64_t key);
void table_foreach(const Table *t, TableForeachFunc f);
UNUSED void table_dump(const Table *t); // for debug

#pragma GCC visibility pop

#endif
