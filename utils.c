#include <errno.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

// Misc

size_t idivceil(size_t n, size_t aligned)
{
    return (n + (aligned - 1)) / aligned;
}

size_t iceil(size_t n, size_t aligned)
{
    return idivceil(n, aligned) * aligned;
}

size_t ptrdiff_abs(const void *p, const void *q)
{
    const uint8_t *x = p, *y = q;
    return (x > y) ? x - y : y - x;
}

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

void *xrealloc(void *orig, size_t size)
{
    void *p = realloc(orig, size);
    if (p == NULL)
        error("realloc(.., %zu) failed", size);
    return p;
}

char *xstrdup(const char *s)
{
    char *dup = xmalloc(strlen(s) + 1);
    return strcpy(dup, s);
}

FILE *mopen(const char *s)
{
    errno = 0;
    FILE *fp = fmemopen((char *) s, strlen(s), "r");
    if (fp == NULL) // we do not care that at caller
        error("fmemopen failed: %s", strerror(errno));
    return fp;
}

FILE *mopen_w(char **p)
{
    static size_t dummy;
    errno = 0;
    FILE *fp = open_memstream(p, &dummy);
    if (fp == NULL)
        error("open_memstream failed: %s", strerror(errno));
    return fp;
}

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

static void list_free(List *l)
{
    for (List *p = l, *next; p != NULL; p = next) {
        next = p->next;
        free(p);
    }
}

static List *list_dup_single(const List *l)
{
    if (l == NULL)
        return NULL;
    return list_new(l->key, l->value, l->next);
}

static List *list_dup(const List *l)
{
    if (l == NULL)
        return NULL;
    List *dup = list_dup_single(l);
    for (List *p = dup; l != NULL; l = l->next, p = p->next)
        p->next = list_dup_single(l->next);
    return dup;
}

// Table

enum {
    TABLE_INIT_SIZE = 2,      // must be power of two
    TABLE_RESIZE_FACTOR = 4,  // ditto
    TABLE_TOO_MANY_FACTOR = 2,
};

struct Table {
    size_t size, body_size;
    List **body;
};

const uint64_t TABLE_NOT_FOUND = UINT64_MAX-1;

Table *table_new(void)
{
    Table *t = xmalloc(sizeof(Table));
    t->size = 0;
    t->body_size = 0;
    t->body = NULL;
    return t;
}

void table_free(Table *t)
{
    if (t == NULL)
        return;
    for (size_t i = 0; i < t->body_size; i++)
        list_free(t->body[i]);
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

static double stdev_length(List *const *body, size_t n, double avr)
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

static inline uint64_t table_hash(uint64_t x)
{
    return x * UINT64_C(0x517cc1b727220a95); // fxhash
}

static inline uint64_t body_index(const Table *t, uint64_t key)
{
    return table_hash(key) & (t->body_size - 1U);
}

static void table_resize(Table *t)
{
    const size_t old_body_size = t->body_size;
    List **old_body = t->body;
    t->body_size *= TABLE_RESIZE_FACTOR;
    size_t newsize = t->body_size * sizeof(List *);
    t->body = xmalloc(newsize);
    List **lasts = xmalloc(newsize);
    List *dummies = xmalloc(t->body_size * sizeof(List));
    for (size_t i = 0; i < t->body_size; i++) {
        dummies[i].next = NULL;
        t->body[i] = lasts[i] = &dummies[i];
    }
    for (size_t i = 0, j; i < old_body_size; i++) {
        for (List *p = old_body[i], *next; p != NULL; p = next) {
            next = p->next;
            j = body_index(t, p->key);
            p->next = NULL;
            lasts[j] = lasts[j]->next = p;
        }
    }
    free(old_body);
    free(lasts);
    for (size_t i = 0; i < t->body_size; i++)
        t->body[i] = t->body[i]->next; // ensure not to use dummies
    free(dummies);
}

static inline bool table_too_many_elements(const Table *t)
{
    return t->size > t->body_size * TABLE_TOO_MANY_FACTOR;
}

// `value` can't be TABLE_NOT_FOUND
Table *table_put(Table *t, uint64_t key, uint64_t value)
{
    if (value == TABLE_NOT_FOUND)
        bug("got invalid value == TABLE_NOT_FOUND");
    if (t->body_size == 0) {
        t->body_size = TABLE_INIT_SIZE;
        t->body = xcalloc(TABLE_INIT_SIZE, sizeof(List *)); // set NULL
    } else if (table_too_many_elements(t))
        table_resize(t);
    uint64_t i = body_index(t, key);
    t->body[i] = list_new(key, value, t->body[i]); // prepend even if the same key exists
    t->size++;
    return t;
}

static List *find(const Table *t, uint64_t key)
{
    uint64_t i = body_index(t, key);
    for (List *p = t->body[i]; p != NULL; p = p->next) {
        if (p->key == key) // direct
            return p;
    }
    return NULL;
}

uint64_t table_get(const Table *t, uint64_t key)
{
    if (t->body_size == 0)
        return TABLE_NOT_FOUND;
    const List *p = find(t, key);
    return p == NULL ? TABLE_NOT_FOUND : p->value;
}

bool table_set(Table *t, uint64_t key, uint64_t value)
{
    if (value == TABLE_NOT_FOUND)
        bug("got invalid value == TABLE_NOT_FOUND");
    if (t->body_size == 0)
        return false;
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

Table *table_dup(const Table *t)
{
    Table *u = xmalloc(sizeof(Table));
    *u = *t;
    u->body = xmalloc(sizeof(List *) * t->body_size);
    for (size_t i = 0; i < t->body_size; i++)
        u->body[i] = list_dup(t->body[i]);
    return u;
}
