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
    TAG_LAST = TAG_CONTINUATION
} ValueTag;

typedef struct {
    Value car, cdr;
} Pair;

typedef struct LocatedPair {
    Value car, cdr; // the same as Pair
    int64_t pos; // value from ftell(3)
} LocatedPair;

typedef Value (*cfunc_t)(/*ANYARGS*/);
typedef struct {
    int64_t arity;
    cfunc_t cfunc;
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
    Value call_stack;
    jmp_buf regs;
} ExecutionState;

typedef struct {
    int64_t arity;
    uintptr_t *sp;
    Value retval;
    ExecutionState *state;
} Continuation;

typedef struct {
    ValueTag tag; // common
    union {
        char *string;
        Pair pair;
        LocatedPair lpair;
        CFunc cfunc;
        Closure closure;
        Continuation continuation;
    };
} SchObject;

#define OBJ(v) ((SchObject *) v)
#define STRING(v) (OBJ(v)->string)
#define PAIR(v) (&OBJ(v)->pair)
#define LOCATED_PAIR(v) (&OBJ(v)->lpair)
#define CFUNC(v) (&OBJ(v)->cfunc)
#define CLOSURE(v) (&OBJ(v)->closure)
#define CONTINUATION(v) (&OBJ(v)->continuation)

extern Value SYM_QUOTE, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING;

ATTR_HIDDEN Value iparse(FILE *in, const char *filename);
ATTR_HIDDEN void pos_to_line_col(int64_t pos, Value newline_pos, int64_t *line, int64_t *col);
ATTR_HIDDEN ATTR(noreturn) void raise_error(jmp_buf buf, const char *fmt, ...);
ATTR_HIDDEN Value reverse(Value l);
ATTR_HIDDEN SchObject *obj_new(ValueTag t);

ATTR_HIDDEN void gc_init(uintptr_t *base_sp);
ATTR_HIDDEN void gc_fin(void);

ATTR_HIDDEN size_t gc_stack_get_size(uintptr_t *sp);
ATTR_HIDDEN ATTR_XMALLOC void *gc_malloc(size_t size);

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
