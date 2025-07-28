#include <stdio.h>

#include "schaf.h"

#define MAP_8(c, x, ...) c(x) __VA_OPT__("Too many arguments!")
#define MAP_7(c, x, ...) c(x) __VA_OPT__(, MAP_8(c, __VA_ARGS__))
#define MAP_6(c, x, ...) c(x) __VA_OPT__(, MAP_7(c, __VA_ARGS__))
#define MAP_5(c, x, ...) c(x) __VA_OPT__(, MAP_6(c, __VA_ARGS__))
#define MAP_4(c, x, ...) c(x) __VA_OPT__(, MAP_5(c, __VA_ARGS__))
#define MAP_3(c, x, ...) c(x) __VA_OPT__(, MAP_4(c, __VA_ARGS__))
#define MAP_2(c, x, ...) c(x) __VA_OPT__(, MAP_3(c, __VA_ARGS__))
#define MAP_1(c, x, ...) c(x) __VA_OPT__(, MAP_2(c, __VA_ARGS__))
#define MAP(c, ...)           __VA_OPT__(MAP_1(c, __VA_ARGS__))

// MAP2: (args).repeated_permutation(2).map{ c(it) }
#define MAP2x(c, x, y) c(x, y), c(y, x)
#define MAP2_4(c, x, y, z, w, ...) c(w, w), MAP2x(c, x, w), MAP2x(c, y, w), MAP2x(c, z, w) \
        __VA_OPT__("Too many arguments!")
#define MAP2_3(c, x, y, z, ...) c(z, z), MAP2x(c, x, z), MAP2x(c, y, z) \
        __VA_OPT__(, MAP2_4(c, x, y, z, __VA_ARGS__))
#define MAP2_2(c, x, y, ...) c(y, y), MAP2x(c, x, y) \
        __VA_OPT__(, MAP2_3(c, x, y, __VA_ARGS__))
#define MAP2_1(c, x, ...) c(x, x) __VA_OPT__(, MAP2_2(c, x, __VA_ARGS__))
#define MAP2(c, ...) __VA_OPT__(MAP2_1(c, __VA_ARGS__))

// MAP3: (args).repeated_permutation(3).map{ c(it) }
#define MAP3x3(c, x, y, z) c(x, y, z), c(x, z, y), c(y, x, z), c(y, z, x), c(z, x, y), c(z, y, x)
#define MAP3x2(c, x, y) c(x, x, y), c(x, y, x), c(x, y, y), c(y, x, x), c(y, x, y), c(y, y, x)
#define MAP3_3(c, x, y, z, ...) c(z, z, z), MAP3x2(c, x, z), MAP3x2(c, y, z), MAP3x3(c, x, y, z) \
        __VA_OPT__("Too many arguments!")
#define MAP3_2(c, x, y, ...) c(y, y, y), MAP3x2(c, x, y) \
        __VA_OPT__(, MAP3_3(c, x, y, __VA_ARGS__))
#define MAP3_1(c, x, ...) c(x, x, x) __VA_OPT__(, MAP3_2(c, x, __VA_ARGS__))
#define MAP3(c, ...) __VA_OPT__(MAP3_1(c, __VA_ARGS__))

#define MAPARG_8(c, a, x, ...) c(a, x) __VA_OPT__("Too many arguments!")
#define MAPARG_7(c, a, x, ...) c(a, x) __VA_OPT__(, MAPARG_8(c, a, __VA_ARGS__))
#define MAPARG_6(c, a, x, ...) c(a, x) __VA_OPT__(, MAPARG_7(c, a, __VA_ARGS__))
#define MAPARG_5(c, a, x, ...) c(a, x) __VA_OPT__(, MAPARG_6(c, a, __VA_ARGS__))
#define MAPARG_4(c, a, x, ...) c(a, x) __VA_OPT__(, MAPARG_5(c, a, __VA_ARGS__))
#define MAPARG_3(c, a, x, ...) c(a, x) __VA_OPT__(, MAPARG_4(c, a, __VA_ARGS__))
#define MAPARG_2(c, a, x, ...) c(a, x) __VA_OPT__(, MAPARG_3(c, a, __VA_ARGS__))
#define MAPARG_1(c, a, x, ...) c(a, x) __VA_OPT__(, MAPARG_2(c, a, __VA_ARGS__))
#define MAPARG(c, a, ...)              __VA_OPT__(MAPARG_1(c, a, __VA_ARGS__))

#define MAPARG_TWIN_3(c, a, x, y, ...) c(a, x, y), c(a, y, x), c(a, y, y) \
        __VA_OPT__("Too many arguments!")
#define MAPARG_TWIN_2(c, a, x, y, ...) c(a, x, y), c(a, y, x), c(a, y, y) \
        __VA_OPT__(, MAPARG_TWIN_3(c, a, y, __VA_ARGS__))
#define MAPARG_TWIN_1(c, a, x, ...) c(a, x, x) \
        __VA_OPT__(, MAPARG_TWIN_2(c, a, x, __VA_ARGS__))
#define MAPARG_TWIN(c, a, ...)  __VA_OPT__(MAPARG_TWIN_1(c, a, __VA_ARGS__))

#define MAPARG_TRIP_3(c, a, x, y, z, ...) c(a, z, z, z), \
        c(a, x, x, z), c(a, x, y, z), c(a, y, x, z), c(a, y, y, z), \
        c(a, x, z, x), c(a, x, z, y), c(a, y, z, x), c(a, y, z, y), \
        c(a, z, x, x), c(a, z, x, y), c(a, z, y, x), c(a, z, y, y), \
        c(a, x, z, z), c(a, y, z, z), c(a, z, z, x), c(a, z, z, y), \
        c(a, z, x, z), c(a, z, y, z) \
        __VA_OPT__("Too many arguments!")
#define MAPARG_TRIP_2(c, a, x, y, ...) c(a, y, y, y), \
        c(a, x, x, y), c(a, x, y, x), c(a, x, y, y), \
        c(a, y, x, x), c(a, y, x, y), c(a, y, y, x) \
        __VA_OPT__(, MAPARG_TRIP_3(c, a, x, y, __VA_ARGS__))
#define MAPARG_TRIP_1(c, a, x, ...) c(a, x, x, x) \
        __VA_OPT__(, MAPARG_TRIP_2(c, a, x, __VA_ARGS__))
#define MAPARG_TRIP(c, a, ...)  __VA_OPT__(MAPARG_TRIP_1(c, a, __VA_ARGS__))

#define UNWRAP(...) __VA_ARGS__

#define MAP2_TWIN_6(c, ys, x, ...) MAPARG(c, x, UNWRAP ys) \
        __VA_OPT__("Too many arguments!")
#define MAP2_TWIN_5(c, ys, x, ...) MAPARG(c, x, UNWRAP ys) \
        __VA_OPT__(, MAP2_TWIN_6(c, ys, __VA_ARGS__))
#define MAP2_TWIN_4(c, ys, x, ...) MAPARG(c, x, UNWRAP ys) \
        __VA_OPT__(, MAP2_TWIN_5(c, ys, __VA_ARGS__))
#define MAP2_TWIN_3(c, ys, x, ...) MAPARG(c, x, UNWRAP ys) \
        __VA_OPT__(, MAP2_TWIN_4(c, ys, __VA_ARGS__))
#define MAP2_TWIN_2(c, ys, x, ...) MAPARG(c, x, UNWRAP ys) \
        __VA_OPT__(, MAP2_TWIN_3(c, ys, __VA_ARGS__))
#define MAP2_TWIN_1(c, ys, ...) __VA_OPT__(MAP2_TWIN_2(c, ys, __VA_ARGS__))
#define MAP2_TWIN(c, xs, ys) MAP2_TWIN_1(c, ys, UNWRAP xs)

#define MAP3_TWIN_6(c, ys, x, ...) MAPARG_TWIN(c, x, UNWRAP ys) \
        __VA_OPT__("Too many arguments!")
#define MAP3_TWIN_5(c, ys, x, ...) MAPARG_TWIN(c, x, UNWRAP ys) \
        __VA_OPT__(, MAP3_TWIN_6(c, ys, __VA_ARGS__))
#define MAP3_TWIN_4(c, ys, x, ...) MAPARG_TWIN(c, x, UNWRAP ys) \
        __VA_OPT__(, MAP3_TWIN_5(c, ys, __VA_ARGS__))
#define MAP3_TWIN_3(c, ys, x, ...) MAPARG_TWIN(c, x, UNWRAP ys) \
        __VA_OPT__(, MAP3_TWIN_4(c, ys, __VA_ARGS__))
#define MAP3_TWIN_2(c, ys, x, ...) MAPARG_TWIN(c, x, UNWRAP ys) \
        __VA_OPT__(, MAP3_TWIN_3(c, ys, __VA_ARGS__))
#define MAP3_TWIN_1(c, ys, ...) __VA_OPT__(MAP3_TWIN_2(c, ys, __VA_ARGS__))
#define MAP3_TWIN(c, xs, ys) MAP3_TWIN_1(c, ys, UNWRAP xs)

#define MAP4_TWIN_6(c, ys, x, ...) MAPARG_TRIP(c, x, UNWRAP ys) \
        __VA_OPT__("Too many arguments!")
#define MAP4_TWIN_5(c, ys, x, ...) MAPARG_TRIP(c, x, UNWRAP ys) \
        __VA_OPT__(, MAP4_TWIN_6(c, ys, __VA_ARGS__))
#define MAP4_TWIN_4(c, ys, x, ...) MAPARG_TRIP(c, x, UNWRAP ys) \
        __VA_OPT__(, MAP4_TWIN_5(c, ys, __VA_ARGS__))
#define MAP4_TWIN_3(c, ys, x, ...) MAPARG_TRIP(c, x, UNWRAP ys) \
        __VA_OPT__(, MAP4_TWIN_4(c, ys, __VA_ARGS__))
#define MAP4_TWIN_2(c, ys, x, ...) MAPARG_TRIP(c, x, UNWRAP ys) \
        __VA_OPT__(, MAP4_TWIN_3(c, ys, __VA_ARGS__))
#define MAP4_TWIN_1(c, ys, ...) __VA_OPT__(MAP4_TWIN_2(c, ys, __VA_ARGS__))
#define MAP4_TWIN(c, xs, ys) MAP4_TWIN_1(c, ys, UNWRAP xs)

#define TYPES_ARG Value, int64_t, const char*
#define TYPES_RET TYPES_ARG, void, bool
#define TY3_PAT(t, u, v, w) t (*)(Value, u, v, w): 3
#define TY2_PAT(t, u, v) t (*)(Value, u, v): 2
#define TY1_PAT(t, u) t (*)(Value, u): 1
#define TY0_PAT(t) t (*)(Value): 0
#define TY3 MAP4_TWIN(TY3_PAT, (TYPES_RET), (TYPES_ARG))
#define TY2 MAP3_TWIN(TY2_PAT, (TYPES_RET), (TYPES_ARG))
#define TY1 MAP2_TWIN(TY1_PAT, (TYPES_RET), (TYPES_ARG))
#define TY0 MAP(TY0_PAT, TYPES_RET)
#define PATS TY0, TY1, TY2, TY3
#define ARITY(f) _Generic((f), PATS, default: -1)

Value f(Value env);
Value g(Value env, const char *s);
Value h(Value env, int64_t i);
bool i(Value env, int64_t i, const char *s);
Value j(Value env, int64_t i, const char *s, Value v);
bool k(Value env);
void l(Value env);

#define print_arity(f) printf(#f": %d\n", ARITY(f))

int main(void)
{
    print_arity(f);
    print_arity(g);
    print_arity(h);
    print_arity(i);
    print_arity(j);
    print_arity(k);
    print_arity(l);
}

