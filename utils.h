#ifndef UTILS_H
#define UTILS_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define ATTR(x) __attribute__((x))
#define UNREACHABLE() error("unreachable"), __builtin_unreachable()
#define ATTR_XMALLOC ATTR(malloc) ATTR(used) ATTR(returns_nonnull)
#define ATTR_HIDDEN ATTR(visibility("hidden"))

ATTR(noreturn) ATTR(format(printf, 1, 2)) void error(const char *fmt, ...);
ATTR_XMALLOC void *xmalloc(size_t size);
ATTR_XMALLOC void *xcalloc(size_t nmem, size_t memsize);

#define debug(fmt, ...) fprintf(stderr, fmt "\n" __VA_OPT__(,) __VA_ARGS__);

typedef struct Table Table;
typedef void (*TableForeachFunc)(uint64_t key, uint64_t val, void *data);
typedef Table Set;
typedef void (*SetForeachFunc)(uint64_t val, void *data);

extern const uint64_t TABLE_NOT_FOUND;
Table *table_new(void);
Table *table_inherit(const Table *t);
void table_free(Table *t);
Table *table_put(Table *t, uint64_t key, uint64_t val); // `val` can't be TABLE_NOT_FOUND
bool table_set(Table *t, uint64_t key, uint64_t val); // set if found
uint64_t table_get(const Table *t, uint64_t key);
bool table_delete(Table *t, uint64_t key);
void table_foreach(const Table *t, TableForeachFunc f, void *data);

Set *set_new(void);
void set_free(Set *s);
Set *set_add(Set *s, uint64_t val);
bool set_include_p(const Set *s, uint64_t val);
bool set_delete(Set *s, uint64_t val);
void set_foreach(const Set *s, SetForeachFunc f, void *data);

#endif
