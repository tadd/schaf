# 0 "<stdin>"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "<stdin>"
# 0 "maptest.h"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "maptest.h"

# 1 "map.h" 1
# 3 "maptest.h" 2




 #define TEST(f) _Generic((f), Value (*)(Value, Value): 1 , Value (*)(Value, int64_t): 1 , Value (*)(Value, const char*): 1 , Value (*)(Value, bool): 1 , default: 0)
