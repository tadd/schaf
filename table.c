#include <string.h>

#include "utils.h"
#include "table.h"

// List

typedef struct List {
    uint64_t key, value;
    struct List *next;
} List;

static List *list_new(uint64_t key, uint64_t value)
{
    List *l = xmalloc(sizeof(List));
    l->key = key;
    l->value = value;
    l->next = NULL;
    return l;
}

static void list_free_until(List *l, const List *const endp)
{
    if (l == NULL)
        return;
    for (List *curr = l, *next = NULL; curr != endp; curr = next) {
        next = curr->next;
        free(curr);
    }
}

static List *list_dup(const List *l, List **last)
{
    List *dup = list_new(l->key, l->value);
    List *prev = dup;
    for (const List *p = l->next; p != NULL; prev = prev->next, p = p->next) {
        prev->next = list_new(p->key, p->value);
    }
    if (last != NULL)
        *last = prev;
    return dup;
}

static List *list_concat(const List *l, List *m)
{
    List *last, *ret = list_dup(l, &last);
    last->next = m;
    return ret;
}

// Table

struct Table {
    List *body;
    const List *inherited;
};

Table *table_new(void)
{
    Table *t = xmalloc(sizeof(Table));
    t->body = NULL;
    t->inherited = NULL;
    return t;
}

Table *table_inherit(Table *p)
{
    Table *t = table_new();
    t->body = p->body;
    t->inherited = p->body;
    return t;
}

Table *table_inherit2(Table *p1, Table *p2)
{
    Table *t = table_new();
    if (p1 == p2)
        t->body = p2->body;
    else
        t->body = list_concat(p1->body, p2->body);
    t->inherited = p2->body;
    return t;
}

Table *table_dup(const Table *src)
{
    Table *t = table_new();
    t->body = list_dup(src->body, NULL);
    return t;
}

void table_free(Table *t)
{
    if (t == NULL)
        return;
    list_free_until(t->body, t->inherited);
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

static List *table_find_pair_1(const List *p, uint64_t key)
{
    for (const List *l = p; l != NULL; l = l->next) {
        if (l->key == key) // direct
            return (List *) l;
    }
    return NULL;
}

// `value` can't be 0
Table *table_put(Table *t, uint64_t key, uint64_t value)
{
    if (value == 0)
        error("%s: got invalid value == 0", __func__);
    List *p = list_new(key, value);
    p->next = t->body;
    t->body = p;
    return t;
}

static inline List *table_find_pair(const Table *t, uint64_t key)
{
    return table_find_pair_1(t->body, key);
}

uint64_t table_get(const Table *t, uint64_t key)
{
    const List *l = table_find_pair(t, key);
    if (l == NULL) // not found
        return 0;
    return l->value;
}

bool table_set(Table *t, uint64_t key, uint64_t value)
{
    if (value == 0)
        error("%s: got invalid value == 0", __func__);
    List *l = table_find_pair(t, key);
    if (l == NULL)
        return false; // did nothing
    l->value = value; // overwrite!
    return true;
}
