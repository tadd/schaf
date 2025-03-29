// intended no include guard

#ifndef DEF
#define DEF(id, noperand, npop, npush) static const char *OP_##id = #id"_"#noperand"_"#npop"_"#npush;
#endif

DEF(nop, 0, 0, 0)

DEF(if, 0, 3, 1)
DEF(jump, 0, 1, 0)
DEF(return, 0, 1, 0)

DEF(apply, 0, 2, 1)

DEF(drop, 0, 1, 0)
DEF(dup, 0, 1, 2)

DEF(get, 0, 1, 1)
DEF(set, 0, 2, 0)

DEF(const, 1, 1, 1)
DEF(null, 0, 0, 1)
DEF(undef, 0, 0, 1)
DEF(false, 0, 0, 1)
DEF(true, 0, 0, 1)

DEF(define, 0, 2, 0) // both function and variable

DEF(and, 0, 2, 1)
DEF(or, 0, 2, 1)

#if 1 // ??
DEF(eq, 0, 2, 1)
DEF(ne, 0, 2, 1)
DEF(lt, 0, 2, 1)
DEF(le, 0, 2, 1)
DEF(gt, 0, 2, 1)
DEF(ge, 0, 2, 1)

DEF(add, 0, 2, 1)
DEF(sub, 0, 2, 1)
DEF(mul, 0, 2, 1)
DEF(div, 0, 2, 1)
DEF(mod, 0, 2, 1)
DEF(inc, 0, 1, 1)
DEF(dec, 0, 1, 1)
DEF(neg, 0, 1, 1)

DEF(not, 0, 1, 1)
DEF(xor, 0, 2, 1)
#endif
