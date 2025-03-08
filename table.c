#include <stdint.h>
#include <string.h>

#include "utils.h"
#include "table.h"

// List

typedef struct List {
    uint64_t key, value;
    struct List *next;
} List;

typedef void (*TableFreeFunc)(uint64_t p);
#define FREE(f, x) ((f) != NULL ? f(x) : (void) 0)

static List *list_new(uint64_t key, uint64_t value, List *next)
{
    List *l = xmalloc(sizeof(List));
    l->key = key;
    l->value = value;
    l->next = next;
    return l;
}

static void list_free(List *l, TableFreeFunc free_key)
{
    for (List *curr = l, *next = NULL; curr != NULL; curr = next) {
        next = curr->next;
        FREE(free_key, curr->key);
        free(curr);
    }
}

// Table

typedef bool (*TableEqualFunc)(uint64_t x, uint64_t y);
#define EQ(f, x, y) ((f) != NULL ? f(x, y) : ((x) == (y)))

struct Table {
    List *body;
    TableEqualFunc eq;
    TableFreeFunc free_key;
    const Table *parent;
};

const uint64_t TABLE_NOT_FOUND = UINT64_MAX-1;

static Table *table_new_full(TableEqualFunc eq, TableFreeFunc free_key, const Table *p)
{
    Table *t = xmalloc(sizeof(Table));
    t->body = NULL;
    t->eq = eq;
    t->free_key = free_key;
    t->parent = p;
    return t;
}

Table *table_new(void)
{
    return table_new_full(NULL, NULL, NULL);
}

static inline bool str_equal(uint64_t s, uint64_t t)
{
    return strcmp((const char *) s, (const char *) t) == 0;
}

Table *table_new_str(void)
{
    return table_new_full(str_equal,
                          (TableFreeFunc)(void*) free,
                          NULL);
}

Table *table_inherit(const Table *p)
{
    return table_new_full(p->eq, p->free_key, p);
}

void table_free(Table *t)
{
    if (t == NULL)
        return;
    list_free(t->body, t->free_key);
    free(t);
}

#if 0
static size_t list_length(List *l)
{
    size_t len = 0;
    for (; l != NULL; l = l->next)
        len++;
    return len;
}
#endif

// `value` can't be TABLE_NOT_FOUND
Table *table_put(Table *t, uint64_t key, uint64_t value)
{
    if (value == TABLE_NOT_FOUND)
        error("%s: got invalid value == TABLE_NOT_FOUND", __func__);
    t->body = list_new(key, value, t->body);
    return t;
}

static List *find1(const List *p, uint64_t key, TableEqualFunc eq)
{
    for (const List *l = p; l != NULL; l = l->next) {
        if (EQ(eq, l->key, key))
            return (List *) l;
    }
    return NULL;
}

// chained!
static List *find(const Table *t, uint64_t key)
{
    for (; t != NULL; t = t->parent) {
        List *found = find1(t->body, key, t->eq);
        if (found != NULL)
            return found;
    }
    return NULL;
}

uint64_t table_get(const Table *t, uint64_t key)
{
    const List *l = find(t, key);
    return l == NULL ? TABLE_NOT_FOUND : l->value;
}

bool table_set(Table *t, uint64_t key, uint64_t value)
{
    if (value == TABLE_NOT_FOUND)
        error("%s: got invalid value == TABLE_NOT_FOUND", __func__);
    List *l = find(t, key);
    if (l == NULL)
        return false; // not found; do nothing
    l->value = value; // overwrite!
    return true;
}

void table_foreach(const Table *t, TableForeachFunc f, void *data)
{
    for (; t != NULL; t = t->parent) {
        for (const List *l = t->body; l != NULL; l = l->next) {
            (*f)(l->key, l->value, data);
        }
    }
}
