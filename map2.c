#include <stdio.h>

#include "schaf.h"

#define MAP_4(c, x, ...) c(x) __VA_OPT__("Too many arguments for MAP()!")
#define MAP_3(c, x, ...) c(x) __VA_OPT__(, MAP_4(c, __VA_ARGS__))
#define MAP_2(c, x, ...) c(x) __VA_OPT__(, MAP_3(c, __VA_ARGS__))
#define MAP_1(c, x, ...) c(x) __VA_OPT__(, MAP_2(c, __VA_ARGS__))
#define MAP(c, ...)           __VA_OPT__(MAP_1(c, __VA_ARGS__))

#define MAP2x(c, x, y) c(x, y), c(y, x)
#define MAP2_4(c, x, y, z, w, ...) c(w, w), MAP2x(c, x, w), MAP2x(c, y, w), MAP2x(c, z, w) \
        __VA_OPT__("Too many arguments for MAP2()!")
#define MAP2_3(c, x, y, z, ...) c(z, z), MAP2x(c, x, z), MAP2x(c, y, z) \
        __VA_OPT__(, MAP2_4(c, x, y, z, __VA_ARGS__))
#define MAP2_2(c, x, y, ...) c(y, y), MAP2x(c, x, y) \
        __VA_OPT__(, MAP2_3(c, x, y, __VA_ARGS__))
#define MAP2_1(c, x, ...) c(x, x) __VA_OPT__(, MAP2_2(c, x, __VA_ARGS__))
#define MAP2(c, ...) __VA_OPT__(MAP2_1(c, __VA_ARGS__))

#define TYPES Value, int64_t, const char*
#define TY2_PAT(t, u) Value (*)(Value, t, u): 2
#define TY1_PAT(t) Value (*)(Value, t): 1
#define TY2 MAP2(TY2_PAT, TYPES)
#define TY1 MAP(TY1_PAT, TYPES)
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

