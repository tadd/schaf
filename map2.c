#include <stdio.h>

#include "schaf.h"

#define MAP_5(c, x, ...) c(x) __VA_OPT__("Too many arguments!")
#define MAP_4(c, x, ...) c(x) __VA_OPT__(, MAP_5(c, __VA_ARGS__))
#define MAP_3(c, x, ...) c(x) __VA_OPT__(, MAP_4(c, __VA_ARGS__))
#define MAP_2(c, x, ...) c(x) __VA_OPT__(, MAP_3(c, __VA_ARGS__))
#define MAP_1(c, x, ...) c(x) __VA_OPT__(, MAP_2(c, __VA_ARGS__))
#define MAP(c, ...)           __VA_OPT__(MAP_1(c, __VA_ARGS__))

#define MAPARG_3(c, a, x, ...) c(a, x) __VA_OPT__("Too many arguments!")
#define MAPARG_2(c, a, x, ...) c(a, x) __VA_OPT__(, MAPARG_3(c, a, __VA_ARGS__))
#define MAPARG_1(c, a, x, ...) c(a, x) __VA_OPT__(, MAPARG_2(c, a, __VA_ARGS__))
#define MAPARG(c, a, ...)              __VA_OPT__(MAPARG_1(c, a, __VA_ARGS__))

#define MAPARG_TWIN_EXPAND(c, a, x, y) c(a, x, y), c(a, y, x), c(a, y, y)
#define MAPARG_TWIN_3(c, a, x, y, ...) MAPARG_TWIN_EXPAND(c, a, x, y) \
        __VA_OPT__("Too many arguments!")
#define MAPARG_TWIN_2(c, a, x, y, ...) MAPARG_TWIN_EXPAND(c, a, x, y) \
        __VA_OPT__(, MAPARG_TWIN_3(c, a, y, __VA_ARGS__))
#define MAPARG_TWIN_1(c, a, x, ...) c(a, x, x) \
        __VA_OPT__(, MAPARG_TWIN_2(c, a, x, __VA_ARGS__))
#define MAPARG_TWIN(c, a, ...)  __VA_OPT__(MAPARG_TWIN_1(c, a, __VA_ARGS__))

#define MAPARG_TRIP_EXPAND(c, a, x, y) \
        c(a, x, x, y), c(a, x, y, x), c(a, x, y, y), \
        c(a, y, x, x), c(a, y, x, y), c(a, y, y, x)
#define MAPARG_TRIP_3(c, a, x, y, z, ...) c(a, z, z, z), \
        c(a, x, y, z), c(a, y, z, x), c(a, z, x, y), \
        c(a, y, x, z), c(a, x, z, y), c(a, z, y, x), \
        MAPARG_TRIP_EXPAND(c, a, x, z), MAPARG_TRIP_EXPAND(c, a, y, z) \
        __VA_OPT__("Too many arguments!")
#define MAPARG_TRIP_2(c, a, x, y, ...) c(a, y, y, y), \
        MAPARG_TRIP_EXPAND(c, a, x, y) \
        __VA_OPT__(, MAPARG_TRIP_3(c, a, x, y, __VA_ARGS__))
#define MAPARG_TRIP_1(c, a, x, ...) c(a, x, x, x) \
        __VA_OPT__(, MAPARG_TRIP_2(c, a, x, __VA_ARGS__))
#define MAPARG_TRIP(c, a, ...) __VA_OPT__(MAPARG_TRIP_1(c, a, __VA_ARGS__))

#define UNWRAP(...) __VA_ARGS__

#define MAPx_TWIN_6(c, m, ys, x, ...) m(c, x, UNWRAP ys) \
        __VA_OPT__("Too many arguments!")
#define MAPx_TWIN_5(c, m, ys, x, ...) m(c, x, UNWRAP ys) \
        __VA_OPT__(, MAPx_TWIN_6(c, m, ys, __VA_ARGS__))
#define MAPx_TWIN_4(c, m, ys, x, ...) m(c, x, UNWRAP ys) \
        __VA_OPT__(, MAPx_TWIN_5(c, m, ys, __VA_ARGS__))
#define MAPx_TWIN_3(c, m, ys, x, ...) m(c, x, UNWRAP ys) \
        __VA_OPT__(, MAPx_TWIN_4(c, m, ys, __VA_ARGS__))
#define MAPx_TWIN_2(c, m, ys, x, ...) m(c, x, UNWRAP ys) \
        __VA_OPT__(, MAPx_TWIN_3(c, m, ys, __VA_ARGS__))
#define MAPx_TWIN_1(c, m, ys, ...) __VA_OPT__(MAPx_TWIN_2(c, m, ys, __VA_ARGS__))
#define MAPx_TWIN(c, m, xs, ys) MAPx_TWIN_1(c, m, ys, UNWRAP xs)

#define MAP2_TWIN(c, xs, ys) MAPx_TWIN(c, MAPARG, xs, ys)
#define MAP3_TWIN(c, xs, ys) MAPx_TWIN(c, MAPARG_TWIN, xs, ys)
#define MAP4_TWIN(c, xs, ys) MAPx_TWIN(c, MAPARG_TRIP, xs, ys)

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

