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
    for (List *next; l != NULL; l = next) {
        next = l->next;
        FREE(free_key, l->key);
        free(l);
    }
}

// Table

enum {
    TABLE_INIT_SIZE = 2,
    TABLE_TOO_MANY_FACTOR = 3,
};

typedef bool (*TableEqualFunc)(uint64_t x, uint64_t y);
typedef uint64_t (*TableHashFunc)(uint64_t x);
#define EQ(f, x, y) ((f) != NULL ? f(x, y) : ((x) == (y)))

struct Table {
    const Table *parent;
    size_t size, body_size;
    TableEqualFunc eq;
    TableHashFunc hash;
    TableFreeFunc free_key;
    List **body;
};

const uint64_t TABLE_NOT_FOUND = UINT64_MAX-1;

static uint64_t direct_hash(uint64_t x) // simplified xorshift
{
    x ^= x << 7U;
    x ^= x >> 9U;
    return x;
}

static Table *table_new_full(TableEqualFunc eq, TableHashFunc hash, TableFreeFunc free_key,
                             const Table *p)
{
    Table *t = xmalloc(sizeof(Table));
    t->size = 0;
    t->body_size = TABLE_INIT_SIZE;
    t->body = xcalloc(TABLE_INIT_SIZE, sizeof(List *)); // set NULL
    t->eq = eq;
    t->hash = hash;
    t->free_key = free_key;
    t->parent = p;
    return t;
}

Table *table_new(void)
{
    return table_new_full(NULL, direct_hash, NULL, NULL);
}

static inline bool str_equal(uint64_t s, uint64_t t)
{
    return strcmp((const char *) s, (const char *) t) == 0;
}

static uint64_t str_hash(uint64_t x) // modified djb2
{
    uint64_t h = 30011U;
    for (const char *s = (char *) x; *s != '\0'; s++)
        h = h * 61U + *s;
    return h;
}

Table *table_new_str(void)
{
    return table_new_full(str_equal, str_hash,
                          (TableFreeFunc)(void*) free,
                          NULL);
}

Table *table_inherit(const Table *p)
{
    return table_new_full(p->eq, p->hash, p->free_key, p);
}

void table_free(Table *t)
{
    if (t == NULL)
        return;
    for (size_t i = 0; i < t->body_size; i++)
        list_free(t->body[i], t->free_key);
    free(t->body);
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

void table_dump(const Table *t)
{
    fprintf(stderr, "size, body_size: %zu, %zu\n", t->size, t->body_size);
    for (size_t i = 0; i < t->body_size; i++) {
        fprintf(stderr, "[%2zu]: %zu\n", i, list_length(t->body[i]));
    }
}
#endif

static inline List **table_body(const Table *t, uint64_t key)
{
    uint64_t i = t->hash(key) % t->body_size;
    return &t->body[i];
}

static inline bool table_too_many_elements(const Table *t)
{
    return t->size > t->body_size * TABLE_TOO_MANY_FACTOR;
}

static void list_prepend(List **p, List *l)
{
    l->next = *p;
    *p = l;
}

static List *list_reverse(List *l)
{
    List *prev = NULL;
    for (List *next; l != NULL; prev = l, l = next) {
        next = l->next;
        l->next = prev;
    }
    return prev;
}

// "next" is twice or more larger than `curr`
static size_t next_size(size_t curr)
{
    static const size_t prime_max = 823117;
    static const size_t primes[] = {
        1, 2, 5, 11, 23, 47, 97, 197, 397, 797, 1597, 3203, 6421,
        12853, 25717, 51437, 102877, 205759, 411527, prime_max,
    };
    static const size_t size = sizeof(primes) / sizeof(primes[0]);

    if (prime_max <= curr)
        goto last;
    for (size_t i = 0; i < size; i++) {
        if (primes[i] > curr)
            return primes[i];
    }
 last:
    return curr*2+1;
}

static void table_resize(Table *t)
{
    const size_t old_body_size = t->body_size;
    List **old_body = t->body;
    t->body_size = next_size(t->body_size);
    t->body = xcalloc(t->body_size, sizeof(List *)); // set NULL
    for (size_t i = 0; i < old_body_size; i++) {
        List *l = old_body[i];
        for (List *next; l != NULL; l = next) {
            next = l->next;
            l->next = NULL;
            List **p = table_body(t, l->key);
            list_prepend(p, l);
        }
    }
    free(old_body);
    for (size_t i = 0; i < t->body_size; i++)
        t->body[i] = list_reverse(t->body[i]);
}

// `value` can't be TABLE_NOT_FOUND
Table *table_put(Table *t, uint64_t key, uint64_t value)
{
    if (value == TABLE_NOT_FOUND)
        error("%s: got invalid value == TABLE_NOT_FOUND", __func__);
    if (table_too_many_elements(t))
        table_resize(t);
    List **p = table_body(t, key);
    *p = list_new(key, value, *p); // prepend even if the same key exists
    t->size++;
    return t;
}

static List *find1(const List *p, uint64_t key, TableEqualFunc eq)
{
    for (; p != NULL; p = p->next) {
        if (EQ(eq, p->key, key))
            return (List *) p;
    }
    return NULL;
}

// chained!
static List *find(const Table *t, uint64_t key)
{
    for (; t != NULL; t = t->parent) {
        const List *b = *table_body(t, key);
        List *found = find1(b, key, t->eq);
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
    for (size_t i = 0; i < t->body_size; i++) {
        for (const List *l = t->body[i]; l != NULL; l = l->next) {
            (*f)(l->key, l->value, data);
        }
    }
}
