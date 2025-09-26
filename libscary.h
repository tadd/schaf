#ifndef SCARY_H
#define SCARY_H

#include <stddef.h>
#include <stdint.h>

void *scary_new(size_t size);
void *scary_new_sized(size_t nmemb, size_t size);
void scary_free(void *ary);
size_t scary_length(const void *ary);
void scary_pop(void *ary);
#if 0
<T> void scary_push(T **ary, T elem);
<T> T *scary_dup(T *src);
#endif

#define xTYPE0(f, t, id, s1, s2) f(t s1, id##s2)
#define xTYPE2(f, t, id, s1, s2) xTYPE0(f, t, id, s1, s2) xTYPE0(f, unsigned t, u##id, s1, s2)
#define xTYPE1(f, t, s1, s2) xTYPE2(f, t, t, s1, s2)
#define xTYPE(f, s1, s2) \
    xTYPE1(f, char, s1, s2) \
    xTYPE1(f, short, s1, s2) \
    xTYPE1(f, int, s1, s2) \
    xTYPE1(f, long, s1, s2) \
    xTYPE2(f, long long, longlong, s1, s2)
#define xDATA(f) xTYPE(f,,)
#define xPTRS(f) xTYPE(f, *, p) xTYPE0(f, void, void, *, p)
#define xTYPES(f) xDATA(f) xPTRS(f)

#define xDECL_PUSH(type, ident) void scary_push_##ident(type **, const type);
xTYPES(xDECL_PUSH);
#define xPAT_PUSH(type, ident) , type: scary_push_##ident
#define scary_push(pary, elem) _Generic(elem xTYPES(xPAT_PUSH))(pary, elem)

#define xDECL_DUP(type, ident) type *scary_dup_##ident(const type *);
xTYPES(xDECL_DUP);
#define xPAT_DUP(type, ident) , type *: scary_dup_##ident
#define scary_dup(p) _Generic(p xTYPES(xPAT_DUP))(p)

#endif
