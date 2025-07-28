#include <stdio.h>

#include "schaf.h"

#define MAP4(c, x, ...)  c(x) __VA_OPT__("Too many arguments for MAP()!")
#define MAP3(c, x, ...)  c(x) __VA_OPT__(, MAP4(c, __VA_ARGS__))
#define MAP2(c, x, ...)  c(x) __VA_OPT__(, MAP3(c, __VA_ARGS__))
#define MAP(c, x, ...)   c(x) __VA_OPT__(, MAP2(c, __VA_ARGS__))
#define TYPES int64_t, bool, const char*
#define TY2_t(t, u) Value (*)(Value, t, u): 2
#define xTY2(t) Value (*)(Value, t, t): 2
#define xTY1(t) Value (*)(Value, t): 1
#define TY2 MAP(xTY2, Value, TYPES)
#define TY1 MAP(xTY1, Value, TYPES)
#define TY0 Value (*)(Value): 0
#define PATS TY0, TY1, TY2
#define ARITY(f) _Generic((f), PATS, default: -1)

Value f(Value env);
Value g(Value env, const char *s);
Value h(Value env, int64_t i);
Value i(Value env, int64_t i, const char *s);

int main(void)
{
    printf("f: %d\n", ARITY(f));
    printf("g: %d\n", ARITY(g));
    printf("h: %d\n", ARITY(h));
    printf("i: %d\n", ARITY(i));
}

