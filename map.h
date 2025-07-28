#ifndef MAP_H
#define MAP_H

#define EVAL0(a) a
#define EVAL1(a) EVAL0(EVAL0(EVAL0(a)))
#define EVAL2(a) EVAL1(EVAL1(EVAL1(a)))
#define EVAL3(a) EVAL2(EVAL2(EVAL2(a)))
#define EVAL4(a) EVAL3(EVAL3(EVAL3(a)))
#define EVAL(a)  EVAL4(EVAL4(EVAL4(a)))

#define MAP_END(...)
#define MAP_OUT

#define MAP_GET_END2()  0, MAP_END
#define MAP_GET_END1(a) MAP_GET_END2
#define MAP_GET_END(a)  MAP_GET_END1
#define MAP_NEXT0(test, next, ...) next MAP_OUT
#define MAP_NEXT1(test, next) MAP_NEXT0(test, next, 0)
#define MAP_NEXT(test, next)  MAP_NEXT1(MAP_GET_END test, next)

#define MAPN(n, f, x, peek, ...) f(x) MAP_NEXT(peek, MAP##n)(f, peek, __VA_ARGS__)
#define MAP0(...) MAPN(1, __VA_ARGS__)
#define MAP1(...) MAPN(0, __VA_ARGS__)

#define MAP(f, ...) EVAL(MAP1(f, __VA_ARGS__, ()()(), 0))

#endif
