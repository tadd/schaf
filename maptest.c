#include <stdio.h>

#include "schaf.h"
#include "maptest2.h"

Value f(Value env);
Value g(Value env, bool b);
Value h(Value env, int64_t i);
Value i(Value env, long j, bool b);

int main(void)
{
   printf("f: %d\n", TEST(f));
   printf("g: %d\n", TEST(g));
   printf("h: %d\n", TEST(h));
   printf("i: %d\n", TEST(i));
}
