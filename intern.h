#ifndef INTERN_H
#define INTERN_H

#include <setjmp.h>

#include "schaf.h"
#include "utils.h"

typedef enum {
    TAG_PAIR,
    TAG_STRING,
    TAG_CFUNC,
    TAG_SYNTAX, // almost a C Function
    TAG_CLOSURE,
    TAG_CONTINUATION,
    TAG_ENV,
    // internal use only
    TAG_ERROR,
    TAG_LAST = TAG_ERROR
} ValueTag;

typedef struct {
    unsigned tag : 3;
    unsigned immutable : 1;
} Header;

typedef struct {
    Value car, cdr;
} Pair;

typedef struct {
    Pair pair;   // inherit
    int64_t pos; // value from ftell(3)
} LocatedPair;

typedef struct {
    char *body;
} String;

typedef struct {
    int64_t arity;
    Value (*apply)(Value env, Value proc, Value args);
} Procedure;

typedef struct CFunc {
    Procedure proc;
    char *name;
    union {
        void *cfunc;
        Value (*f0)(Value);
        Value (*f1)(Value, Value);
        Value (*f2)(Value, Value, Value);
        Value (*f3)(Value, Value, Value, Value);
    };
} CFunc;

typedef struct {
    Procedure proc;
    Value env;
    Value params;
    Value body;
} Closure;

typedef struct {
    Procedure proc;
    uintptr_t *sp;
    void *stack;
    size_t stack_len;
    jmp_buf state;
    Value retval;
} Continuation;

typedef struct {
    char *name;
    Value parent;
    Table *table;
} Env;

typedef struct {
    Value call_stack; // list of '(func-name . location)
} Error;

#define HEADER(v) (((uintptr_t)(v)) >> 48U)
#define VALUE_TAG(v) (HEADER(v) >> 1U)
#define IMMUTABLE(v) (HEADER(v) & 1U)

#define PTR(v) ((void *)(((uintptr_t)(v)) & UINT64_C(0xFFFF'FFFF'FFFF)))
#define PAIR(v) ((Pair *) PTR(v))
#define LOCATED_PAIR(v) ((LocatedPair *) PTR(v))
#define STRING(v) ((String *) PTR(v))
#define PROCEDURE(v) ((Procedure *) PTR(v))
#define CFUNC(v) ((CFunc *) PTR(v))
#define CLOSURE(v) ((Closure *) PTR(v))
#define CONTINUATION(v) ((Continuation *) PTR(v))
#define ENV(v) ((Env *) PTR(v))
#define ERROR(v) ((Error *) PTR(v))

#define EMBED_TAG(t) ((uintptr_t)(t) << 49U)
#define EMBED_IMMUTABLE(b) ((uintptr_t)(b) << 48U)
#define EMBED_HEADER(t, b) (EMBED_TAG(t)|EMBED_IMMUTABLE(b))

#pragma GCC visibility push(hidden) // also affects Clang

extern Value SYM_QUOTE, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING;

Value iparse(FILE *in, const char *filename);
void pos_to_line_col(int64_t pos, Value newline_pos, int64_t *line, int64_t *col);
[[gnu::noreturn]] void raise_error(jmp_buf buf, const char *fmt, ...);
Value reverse(Value l);
Value obj_new_immutable(size_t size, ValueTag t);

void gc_init(uintptr_t *base_sp);
void gc_fin(void);

size_t gc_stack_get_size(uintptr_t *sp);
ATTR_XMALLOC void *gc_malloc(size_t size);

#pragma GCC visibility pop

static inline Value list1(Value x)
{
    return cons(x, Qnil);
}

static Value cons_const(Value car, Value cdr)
{
    Value o = obj_new_immutable(sizeof(Pair), TAG_PAIR);
    Pair *p = PAIR(o);
    p->car = car;
    p->cdr = cdr;
    return o;
}

static inline Value list1_const(Value x)
{
    return cons_const(x, Qnil);
}

static inline Value list2_const(Value x, Value y)
{
    return cons_const(x, list1_const(y));
}

#define DUMMY_PAIR() (((Value) &(Pair) {.car = Qundef, .cdr = Qnil }) | EMBED_TAG(TAG_PAIR))

#endif // INTERN_H
