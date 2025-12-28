#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libscary.h"

#define NONNULL(p) do { \
        errno = 0; \
        void *q = (p); \
        if (q == NULL) { \
            perror("scary"); \
            abort(); \
        } \
    } while (0)

static inline void *xmalloc(size_t size)
{
    void *p;
    NONNULL(p = malloc(size));
    return p;
}

static inline void *xrealloc(void *p0, size_t size)
{
    void *p;
    NONNULL(p = realloc(p0, size));
    return p;
}

typedef struct {
    size_t capacity, length, elem_size;
    uint8_t space[];
} Scary;

enum {
    SCARY_OFFSET = offsetof(Scary, space),
    SCARY_INIT = 8,
    SCARY_INC_RATIO = 2,
};

#ifdef __clang__
#pragma clang diagnostic ignored "-Wcast-align"
#endif

static inline void *opaque(Scary *a)
{
    return a->space;
}

static inline Scary *get(const void *p)
{
    const uint8_t *bp = p;
    return (Scary *) (bp - SCARY_OFFSET);
}

static Scary *new_raw(size_t nmemb, size_t elem_size)
{
    size_t cap = nmemb * elem_size;
    Scary *ary = xmalloc(sizeof(Scary) + cap);
    ary->capacity = cap;
    ary->length = 0;
    ary->elem_size = elem_size;
    return ary;
 }

void *scary_new(size_t elem_size)
{
    return opaque(new_raw(SCARY_INIT, elem_size));
}

void *scary_new_sized(size_t nmemb, size_t elem_size)
{
    Scary *ary = new_raw(nmemb, elem_size);
    ary->length = nmemb;
    void *p = opaque(ary);
    memset(p, 0, nmemb * elem_size);
    return p;
}

void scary_free(void *p)
{
    if (p != NULL)
        free(get(p));
}

static Scary *maybe_resize(void *p)
{
    const void **pp = p;
    Scary *ary = get(*pp);
    if (ary->capacity <= ary->length * ary->elem_size) {
        ary->capacity *= SCARY_INC_RATIO;
        ary = xrealloc(ary, sizeof(Scary) + ary->capacity);
        *pp = opaque(ary);
    }
    return ary;
}

size_t scary_length(const void *p)
{
    return get(p)->length;
}

#define DEFINE_PUSH_DATA(type, suffix)        \
void scary_push_##suffix(type **p, type elem) \
{                                             \
    Scary *ary = maybe_resize(p);             \
    type *sp = (type *) ary->space;           \
    sp[ary->length++] = elem;                 \
}
xDATA(DEFINE_PUSH_DATA)

static void scary_push_ptr_any(void *p, const void *elem)
{
    Scary *ary = maybe_resize(p);
    const void **sp = (const void **) ary->space;
    sp[ary->length++] = elem;
}

#define DEFINE_PUSH_PTR(type, suffix)                    \
void scary_push_##suffix(type **p, const type elem)      \
{                                                        \
    scary_push_ptr_any(p, elem);                         \
}
xPTRS(DEFINE_PUSH_PTR)

void scary_pop(void *p)
{
    get(p)->length--; // do not shrink for speed
}

static void *scary_dup_any(const void *p)
{
    Scary *ary = get(p);
    Scary *dup = xmalloc(sizeof(Scary) + ary->capacity);
    *dup = *ary;
    memcpy(dup->space, ary->space, ary->length * ary->elem_size);
    return opaque(dup);
}

#define DEFINE_DUP(type, suffix)        \
type *scary_dup_##suffix(const type *p) \
{                                       \
    return scary_dup_any(p);            \
}
xTYPES(DEFINE_DUP)
