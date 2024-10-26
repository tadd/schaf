#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

void error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(2);
}

void *xmalloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL)
        error("malloc(%zu) failed", size);
    return p;
}

static void *xcalloc(size_t nmem, size_t memsize)
{
    void *p = calloc(nmem, memsize);
    if (p == NULL)
        error("calloc(%zu, %zu) failed", nmem, memsize);
    return p;
}

void *xrealloc(void *q, size_t size)
{
    void *p = realloc(q, size);
    if (p == NULL)
        error("realloc(%p, %zu) failed", q, size);
    return p;
}

char *xstrdup(const char *s)
{
    char *dup = strdup(s);
    if (dup == NULL)
        error("strdup(\"%s\") failed", s);
    return dup;
}

enum {
    TABLE_INIT_SIZE = 1,
    TABLE_TOO_MANY_FACTOR = 3,
};

typedef struct List {
    uint64_t key, value;
    struct List *next;
} List;

struct Table {
    size_t size, body_size;
    List **body;
    TableHashFunc hash;
    TableEqualFunc eq;
    TableFreeFunc free_key;
};

static inline uint64_t direct_hash(uint64_t x) // simplified xorshift
{
    x ^= x << 7U;
    x ^= x >> 9U;
    return x;
}

static inline bool direct_equal(uint64_t x, uint64_t y)
{
    return x == y;
}

static inline void free_nop(ATTR(unused) void *p) { }

Table *table_new(void)
{
    return table_new_full(direct_hash, direct_equal, free_nop);
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
    return table_new_full(str_hash, str_equal, free); // expects strdup for keys
}

#define PTR_OR(x, y) (((x) != NULL) ? (x) : (y))

Table *table_new_full(TableHashFunc hash, TableEqualFunc eq, TableFreeFunc free_key)
{
    Table *t = xmalloc(sizeof(Table));
    t->body = xcalloc(sizeof(List *), TABLE_INIT_SIZE); // set NULL
    t->size = 0;
    t->body_size = TABLE_INIT_SIZE;
    for (size_t i = 0; i < t->body_size; i++)
        t->body[i] = NULL;
    t->hash = PTR_OR(hash, direct_hash);
    t->eq = PTR_OR(eq, direct_equal);
    t->free_key = PTR_OR(free_key, free_nop);
    return t;
}

static List *list_new(uint64_t key, uint64_t value)
{
    List *l = xmalloc(sizeof(List));
    l->key = key;
    l->value = value;
    l->next = NULL;
    return l;
}

static void list_free(List *l, TableFreeFunc free_key)
{
    for (List *next; l != NULL; l = next) {
        next = l->next;
        (*free_key)((void *) l->key);
        free(l);
    }
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

static List *list_reverse(const List *l)
{
    List *ret = NULL;
    for (const List *p = l; p != NULL; p = p->next) {
        List *e = list_new(p->key, p->value);
        e->next = ret;
        ret = e;
    }
    return ret;
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

static inline List **table_find_listp(const Table *t, uint64_t key)
{
    uint64_t i = (*t->hash)(key) % t->body_size;
    return &t->body[i];
}

static inline bool table_too_many_elements(const Table *t)
{
    return t->size > t->body_size * TABLE_TOO_MANY_FACTOR;
}

// "next" is twice or more larger than `curr`
static size_t next_prime(size_t curr)
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
    List *old_body[old_body_size];
    memcpy(old_body, t->body, sizeof(List *) * t->body_size);
    t->body_size = next_prime(t->body_size);
    t->body = xcalloc(sizeof(List *), t->body_size); // set NULL
    for (size_t i = 0; i < old_body_size; i++) {
        for (List *l = old_body[i], *next; l != NULL; l = next) {
            List **p = table_find_listp(t, l->key);
            next = l->next;
            l->next = NULL;
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

static void table_put_raw(Table *t, List **p, uint64_t key, uint64_t value)
{
    List *l = list_new(key, value);
    l->next = *p; // prepend even if the same key exists
    *p = l;
    t->size++;
}

// `value` can't be 0
Table *table_put(Table *t, uint64_t key, uint64_t value)
{
    if (value == 0)
        error("%s: got invalid value == 0", __func__);
    table_ensure_size(t);
    List **p = table_find_listp(t, key);
    table_put_raw(t, p, key, value);
    return t;
}

static List *table_find_pair_raw(List **p, uint64_t key, TableEqualFunc eq)
{
    for (List *l = *p; l != NULL; l = l->next) {
        if ((*eq)(l->key, key))
            return l;
    }
    return NULL;
}

static inline List *table_find_pair(const Table *t, uint64_t key)
{
    List **p = table_find_listp(t, key);
    return table_find_pair_raw(p, key, t->eq);
}

uint64_t table_get(const Table *t, uint64_t key)
{
    const List *l = table_find_pair(t, key);
    if (l == NULL) // not found
        return 0;
    return l->value;
}

bool table_set_or_put(Table *t, uint64_t key, uint64_t value)
{
    if (value == 0)
        error("%s: got invalid value == 0", __func__);
    List **p = table_find_listp(t, key);
    List *l = table_find_pair_raw(p, key, t->eq);
    if (l == NULL) { // to put
        if (table_ensure_size(t))
            p = table_find_listp(t, key); // recalc
        table_put_raw(t, p, key, value);
        return false; // not overwritten
    }
    l->value = value; // overwrite!
    return true;
}

Table *table_merge(Table *dst, const Table *src)
{
    const size_t size = src->body_size;
    for (size_t i = 0; i < size; i++) {
        List *rev = list_reverse(src->body[i]);
        for (List *l = rev; l != NULL; l = l->next)
            table_put(dst, l->key, l->value);
        list_free(rev, free_nop);
    }
    return dst;
}
