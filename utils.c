#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

// Misc

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

void *xcalloc(size_t nmem, size_t memsize)
{
    void *p = calloc(nmem, memsize);
    if (p == NULL)
        error("calloc(%zu, %zu) failed", nmem, memsize);
    return p;
}

char *xstrdup(const char *s)
{
    char *dup = xmalloc(strlen(s) + 1);
    return strcpy(dup, s);
}

// List

typedef struct List {
    uint64_t key, value;
    struct List *next;
} List;

typedef void (*TableFreeFunc)(uint64_t p);

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
    for (List *p = l, *next; p != NULL; p = next) {
        next = p->next;
        free_key(p->key);
        free(p);
    }
}

// Table

enum {
    TABLE_INIT_SIZE = 4,      // must be power of two
    TABLE_RESIZE_FACTOR = 2,  // ditto
    TABLE_TOO_MANY_FACTOR = 3,
};

typedef bool (*TableEqualFunc)(uint64_t x, uint64_t y);
typedef uint64_t (*TableHashFunc)(uint64_t x);

struct Table {
    const Table *parent;
    size_t size, body_size;
    TableEqualFunc eq;
    TableHashFunc hash;
    TableFreeFunc free_key;
    List **body;
};

const uint64_t TABLE_NOT_FOUND = UINT64_MAX-1;

static inline bool direct_eq(uint64_t x, uint64_t y)
{
    return x == y;
}

static uint64_t direct_hash(uint64_t x)
{
    x ^= x << 7;
    x ^= x >> 9;
    return x; // xorshift64-based
}

static inline void nofree([[maybe_unused]] uint64_t x) {}

static Table *table_new_full(TableEqualFunc eq, TableHashFunc hash,
                             TableFreeFunc free_key, const Table *parent)
{
    Table *t = xmalloc(sizeof(Table));
    t->size = 0;
    t->body_size = TABLE_INIT_SIZE;
    t->body = xcalloc(TABLE_INIT_SIZE, sizeof(List *)); // set NULL
    t->parent = parent;
    t->eq = eq;
    t->hash = hash;
    t->free_key = free_key;
    return t;
}

Table *table_new(void)
{
    return table_new_full(direct_eq, direct_hash, nofree, NULL);
}

static inline bool str_equal(uint64_t s, uint64_t t)
{
    return strcmp((const char *) s, (const char *) t) == 0;
}

static uint64_t str_hash(uint64_t x)
{
    uint64_t h = 0;
    for (const char *s = (char *) x; *s != '\0'; s++)
        h = h * 37U + *s;
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

static size_t list_length(const List *l)
{
    size_t len = 0;
    for (const List *p = l; p != NULL; p = p->next)
        len++;
    return len;
}


static double stdev_length(List **body, size_t n, double avr)
{
    double s = 0.0;
    for (size_t i = 0; i < n; i++) {
        double d = list_length(body[i]) - avr;
        s += d * d;
    }
    return sqrt(s / n);
}

void table_dump(const Table *t)
{
    double ratio = (double) t->size / t->body_size;
    double stdev = stdev_length(t->body, t->body_size, ratio);
    fprintf(stderr, "size: %zu, body_size: %zu, ratio: %f, stdev: %f\n",
            t->size, t->body_size, ratio, stdev);
    for (size_t i = 0; i < t->body_size; i++) {
        size_t len = list_length(t->body[i]);
        fprintf(stderr, "%2zu ", len);
        for (size_t j = 0; j < len; j++)
            fprintf(stderr, "*");
        fprintf(stderr, "\n");
    }
}

static inline uint64_t body_index(const Table *t, uint64_t key)
{
    return t->hash(key) & (t->body_size - 1U);
}

static inline bool table_too_many_elements(const Table *t)
{
    return t->size > t->body_size * TABLE_TOO_MANY_FACTOR;
}

static List *list_reverse(List *l)
{
    List *prev = NULL;
    for (List *p = l, *next; p != NULL; prev = p, p = next) {
        next = p->next;
        p->next = prev;
    }
    return prev;
}

static void table_resize(Table *t)
{
    const size_t old_body_size = t->body_size;
    List **old_body = t->body;
    t->body_size *= TABLE_RESIZE_FACTOR;
    t->body = xcalloc(t->body_size, sizeof(List *)); // set NULL
    for (size_t i = 0; i < old_body_size; i++) {
        for (List *p = old_body[i], *next; p != NULL; p = next) {
            next = p->next;
            uint64_t j = body_index(t, p->key);
            p->next = t->body[j];
            t->body[j] = p;
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
    uint64_t i = body_index(t, key);
    t->body[i] = list_new(key, value, t->body[i]); // prepend even if the same key exists
    t->size++;
    return t;
}

static List *find1(const List *l, uint64_t key, TableEqualFunc eq)
{
    for (const List *p = l; p != NULL; p = p->next) {
        if (eq(p->key, key))
            return (List *) p;
    }
    return NULL;
}

// chained!
static List *find(const Table *t, uint64_t key)
{
    for (const Table *p = t; p != NULL; p = p->parent) {
        uint64_t i = body_index(p, key);
        List *found = find1(p->body[i], key, p->eq);
        if (found != NULL)
            return found;
    }
    return NULL;
}

uint64_t table_get(const Table *t, uint64_t key)
{
    const List *p = find(t, key);
    return p == NULL ? TABLE_NOT_FOUND : p->value;
}

bool table_set(Table *t, uint64_t key, uint64_t value)
{
    if (value == TABLE_NOT_FOUND)
        error("%s: got invalid value == TABLE_NOT_FOUND", __func__);
    List *p = find(t, key);
    if (p == NULL)
        return false; // not found; do nothing
    p->value = value; // overwrite!
    return true;
}

void table_foreach(const Table *t, TableForeachFunc f, void *data)
{
    for (size_t i = 0; i < t->body_size; i++) {
        for (const List *p = t->body[i]; p != NULL; p = p->next) {
            (*f)(p->key, p->value, data);
        }
    }
}
