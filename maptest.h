// #include "schaf.h"
#include "map.h"

#define FUNC_ARG_TYPES() Value, int64_t, const char*, bool
#define FUNC_ARG_PAT_1_0(t) Value (*)(Value, t): 1 COMMA
#define FUNC_ARG_PAT_1() MAP(FUNC_ARG_PAT_1_0, FUNC_ARG_TYPES())
DEFINE TEST(f) _Generic((f), FUNC_ARG_PAT_1() default: 0)
