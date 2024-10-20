#include <string.h>

#include "utils.h"
#include "table.h"

// List

typedef struct List {
    uint64_t key, value;
    struct List *next;
} List;

static List *list_new(uint64_t key, uint64_t value, List *next)
{
    List *l = xmalloc(sizeof(List));
    l->key = key;
    l->value = value;
    l->next = next;
    return l;
}

static void list_free(List *l, ATTR(unused) TableFreeFunc free_key)
{
    for (List *curr = l, *next = NULL; curr != NULL; curr = next) {
        next = curr->next;
        /* (*free_key)((void  *) l->key); */
        free(curr);
    }
}

// Table

enum {
    TABLE_INIT_SIZE = 1,
    TABLE_TOO_MANY_FACTOR = 3,
    TABLE_RESIZE_FACTOR = 2,
};

struct Table {
    size_t size, body_size;
    List **body;
    TableHashFunc hash;
    TableEqualFunc eq;
    TableFreeFunc free_key;
    const Table *parent;
};

static inline bool direct_equal(uint64_t x, uint64_t y)
{
    return x == y;
}

static inline uint64_t direct_hash(uint64_t x) // simplified xorshift
{
    x ^= x << 7U;
    x ^= x >> 9U;
    return x;
}

Table *table_new_full(const Table *parent,
                      TableHashFunc hash, TableEqualFunc eq, TableFreeFunc free_key)
{
    Table *t = xmalloc(sizeof(Table));
    t->size = 0;
    t->body_size = TABLE_INIT_SIZE;
    t->body = xcalloc(TABLE_INIT_SIZE, sizeof(List *)); // set NULL
    t->hash = hash;
    t->eq = eq;
    t->free_key = free_key;
    t->parent = parent;
    return t;
}

static uint64_t str_hash(uint64_t x) // modified djb2
{
    uint64_t h = 30011;
    for (const char *s = (char *) x; *s != '\0'; s++)
        h = h * 61 + *s;
    return h;
}

static inline bool str_equal(uint64_t s, uint64_t t)
{
    return strcmp((const char *) s, (const char *) t) == 0;
}

Table *table_new_str(void)
{
    return table_new_full(NULL, str_hash, str_equal, free); // expects strdup for keys
}

static inline void free_nop(ATTR(unused) void *p) { }

Table *table_inherit(const Table *p)
{
    return table_new_full(p, direct_hash, direct_equal, free_nop);
}

Table *table_new(void)
{
    return table_inherit(NULL);
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
        fprintf(stderr, "body[%zu]: %zu\n", i, list_length(t->body[i]));
    }
}
#endif

static inline List **table_body(const Table *t, uint64_t key)
{
    uint64_t i = (*t->hash)(key) % t->body_size;
    return &t->body[i];
}

static inline bool table_too_many_elements(const Table *t)
{
    return t->size > t->body_size * TABLE_TOO_MANY_FACTOR;
}

static void list_append(List **p, List *l)
{
    if (*p == NULL) {
        *p = l;
        return;
    }
    List *q;
    for (q = *p; q->next != NULL; q = q->next)
        ;
    q->next = l;
}

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
    const size_t body_size = t->body_size;
    List *body[body_size];
    memcpy(body, t->body, sizeof(List *) * t->body_size);
    t->body_size = next_size(t->body_size);
    t->body = xrealloc(t->body, sizeof(List *) * t->body_size);
    memset(t->body, 0, sizeof(List *) * t->body_size); // set NULL
    for (size_t i = 0; i < body_size; i++) {
        for (List *l = body[i], *next; l != NULL; l = next) {
            next = l->next;
            l->next = NULL;
            List **p = table_body(t, l->key);
            list_append(p, l);
        }
    }
}

static inline bool table_ensure_size(Table *t)
{
    if (table_too_many_elements(t)) {
        table_resize(t);
        return true;
    }
    return false;
}

static Table *table_put_raw(Table *t, List **p, uint64_t key, uint64_t value)
{
    if (value == 0)
        error("%s: got invalid value == 0", __func__);
    if (table_too_many_elements(t))
        table_resize(t);
    *p = list_new(key, value, *p); // prepend even if the same key exists
    t->size++;
    return t;
}


// `value` can't be 0
Table *table_put(Table *t, uint64_t key, uint64_t value)
{
    if (value == 0)
        error("%s: got invalid value == 0", __func__);
    table_ensure_size(t);
    List **p = table_body(t, key);
    return table_put_raw(t, p, key, value);
}

static List *find1(const List *p, uint64_t key, TableEqualFunc eq)
{
    for (const List *l = p; l != NULL; l = l->next) {
        if ((*eq)(l->key, key))
            return (List *) l;
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
    return l == NULL ? 0 : l->value;
}

bool table_set_or_put(Table *t, uint64_t key, uint64_t value)
{
    if (value == 0)
        error("%s: got invalid value == 0", __func__);
    List **p = table_body(t, key);
    List *l = find1(*p, key, t->eq);
    if (l == NULL) { // to put
        if (table_ensure_size(t))
            p = table_body(t, key); // recalc
        table_put_raw(t, p, key, value);
        return false; // not overwritten
    }
    l->value = value; // overwrite!
    return true;
}

void table_merge(Table *dst, const Table *src)
{
    const size_t size = src->body_size;
    for (size_t i = 0; i < size; i++) {
        for (List *l = src->body[i]; l != NULL; l = l->next)
            table_put(dst, l->key, l->value);
    }
}

bool table_set(Table *t, uint64_t key, uint64_t value)
{
    if (value == 0)
        error("%s: got invalid value == 0", __func__);
    List *l = find(t, key);
    if (l == NULL)
        return false; // not found; do nothing
    l->value = value; // overwrite!
    return true;
}
