#ifndef TABLE_H
#define TABLE_H

#include <stdbool.h>
#include <stdint.h>

typedef struct Table Table;

Table *table_new(void);
Table *table_inherit(const Table *t);
void table_free(Table *t);
Table *table_put(Table *t, uint64_t key, uint64_t val); // `val` can't be 0
bool table_set(Table *t, uint64_t key, uint64_t val); // set (only) if found
uint64_t table_get(const Table *t, uint64_t key);

#endif
