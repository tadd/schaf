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
    TAG_ERROR, // internal use only
    TAG_LAST = TAG_ERROR
} ValueTag;

typedef struct {
    Value car, cdr;
} Pair;

typedef struct {
    Pair pair;   // inherit
    int64_t pos; // value from ftell(3)
} LocatedPair;

typedef struct CFunc {
    int64_t arity;
    char *name;
    Value (*applier)(Table *env, struct CFunc *f, Value args);
    union {
        void *cfunc;
        Value (*f0)(Table *);
        Value (*f1)(Table *, Value);
        Value (*f2)(Table *, Value, Value);
        Value (*f3)(Table *, Value, Value, Value);
    };
} CFunc;

typedef struct {
    int64_t arity;
    Table *env;
    Value params;
    Value body;
} Closure;

typedef struct {
    void *stack;
    size_t stack_len;
    jmp_buf regs;
} ExecutionState;

typedef struct {
    int64_t arity;
    uintptr_t *sp;
    Value retval;
    ExecutionState *exstate;
} Continuation;

typedef struct {
    Value call_stack; // list of '(func-name . location)
} Error;

typedef struct {
    ValueTag tag; // common
    union {
        char *string;
        Pair pair;
        LocatedPair lpair;
        CFunc cfunc;
        Closure closure;
        Continuation continuation;
        Error error;
    };
} SchObject;

#define OBJ(v) ((SchObject *) v)
#define PAIR(v) (&OBJ(v)->pair)
#define LOCATED_PAIR(v) (&OBJ(v)->lpair)
#define STRING(v) (OBJ(v)->string)
#define PROC_ARITY(v) (OBJ(v)->cfunc.arity)
#define CFUNC(v) (&OBJ(v)->cfunc)
#define CLOSURE(v) (&OBJ(v)->closure)
#define CONTINUATION(v) (&OBJ(v)->continuation)
#define ERROR(v) (&OBJ(v)->error)

#pragma GCC visibility push(hidden) // also affects Clang

extern Value SYM_QUOTE, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING;

Value iparse(FILE *in, const char *filename);
void pos_to_line_col(int64_t pos, Value newline_pos, int64_t *line, int64_t *col);
[[gnu::noreturn]] void raise_error(jmp_buf buf, const char *fmt, ...);
Value reverse(Value l);
SchObject *obj_new(ValueTag t);

void gc_init(uintptr_t *base_sp);
void gc_fin(void);

size_t gc_stack_get_size(uintptr_t *sp);
ATTR_XMALLOC void *gc_malloc(size_t size);

#pragma GCC visibility pop

static inline Value list1(Value x)
{
    return cons(x, Qnil);
}

static inline Value list2(Value x, Value y)
{
    return cons(x, list1(y));
}

#define DUMMY_PAIR() ((Value) &(SchObject) { .tag = TAG_PAIR, .pair = { .car = Qundef, .cdr = Qnil } })

#endif // INTERN_H
