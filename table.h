#ifndef TABLE_H
#define TABLE_H

#include <stdbool.h>
#include <stdint.h>

typedef struct Table Table;
typedef bool (*TableEqualFunc)(uint64_t x, uint64_t y);
typedef uint64_t (*TableHashFunc)(uint64_t x);
typedef void (*TableFreeFunc)(void *p);

Table *table_new(void);
Table *table_new_str(void);
Table *table_new_full(TableHashFunc hash, TableEqualFunc eq, TableFreeFunc free_key);
Table *table_inherit(const Table *t);
void table_free(Table *t);
Table *table_put(Table *t, uint64_t key, uint64_t val); // `val` can't be 0
uint64_t table_get(const Table *t, uint64_t key);
bool table_set_or_put(Table *t, uint64_t key, uint64_t val);
bool table_set(Table *t, uint64_t key, uint64_t val); // set only if found
Table *table_merge(Table *dst, const Table *src);
const Table *table_get_parent(const Table *t);
const Table *table_set_parent(Table *t, const Table *parent);

#endif
