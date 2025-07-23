#include <string.h>

#include "intern.h"

#define CHECK_ERROR_F(v, f) do { \
        Value V = (v); \
        if (UNLIKELY(f(V))) \
            return V; \
    } while (0)
#define CHECK_ERROR(v) CHECK_ERROR_F(v, is_error)
#define EXPECT(f, ...) CHECK_ERROR(expect_##f(__VA_ARGS__))

[[gnu::noreturn, gnu::noinline]]
static void jump(Continuation *cont)
{
    memcpy(cont->sp, cont->stack, cont->stack_len);
    longjmp(cont->state, 1);
}

[[gnu::noinline]]
static Value apply_continuation(UNUSED Value env, Value f, Value args)
{
    GET_SP(sp);
    EXPECT(arity, PROCEDURE(f)->arity, args);
    Continuation *cont = CONTINUATION(f);
    cont->retval = PROCEDURE(f)->arity == 1 ? car(args) : args;
    int64_t d = sp - cont->sp;
    if (d < 1)
        d = 1;
    volatile uintptr_t pad[d];
    pad[0] = pad[d-1] = 0; // avoid unused
    jump(cont);
}

Value continuation_new(int64_t n)
{
    Continuation *c = obj_new(TAG_CONTINUATION, sizeof(Continuation));
    c->proc.arity = n; // call/cc: 1, call-with-values: -1
    c->proc.apply = apply_continuation;
    c->retval = Qfalse;
    c->sp = NULL;
    c->stack = NULL;
    c->stack_len = 0;
    return (Value) c;
}

[[gnu::noinline]]
bool continuation_set(Value c)
{
    GET_SP(sp); // must be the first!
    Continuation *cont = CONTINUATION(c);
    cont->sp = sp;
    cont->stack_len = gc_stack_get_size(sp);
    cont->stack = xmalloc(cont->stack_len);
    UNPOISON(sp, cont->stack_len);
    memcpy(cont->stack, sp, cont->stack_len);
    return setjmp(cont->state) != 0;
}
