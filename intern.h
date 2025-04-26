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
    ValueTag tag;
    bool immutable;
} Header;

typedef struct {
    Header header; // common
    Value car, cdr;
} Pair;

typedef struct {
    Pair pair;   // inherit
    int64_t pos; // value from ftell(3)
} LocatedPair;

typedef struct {
    Header header;
    char *body;
} String;

typedef struct {
    Header header;
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
    Header header;
    char *name;
    Value parent;
    Table *table;
} Env;

typedef struct {
    Header header;
    Value call_stack; // list of '(func-name . location)
} Error;

#define HEADER(v) ((Header *) v)
#define VALUE_TAG(v) (HEADER(v)->tag)

#define PAIR(v) ((Pair *) v)
#define LOCATED_PAIR(v) ((LocatedPair *) v)
#define STRING(v) ((String *) v)
#define PROCEDURE(v) ((Procedure *) v)
#define CFUNC(v) ((CFunc *) v)
#define CLOSURE(v) ((Closure *) v)
#define CONTINUATION(v) ((Continuation *) v)
#define ENV(v) ((Env *) v)
#define ERROR(v) ((Error *) v)

#pragma GCC visibility push(hidden) // also affects Clang

extern Value SYM_QUOTE, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING;

Value iparse(FILE *in, const char *filename);
void pos_to_line_col(int64_t pos, Value newline_pos, int64_t *line, int64_t *col);
[[gnu::noreturn]] void raise_error(jmp_buf buf, const char *fmt, ...);
Value reverse(Value l);
void *obj_new(size_t size, ValueTag t);

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
    Pair *p = obj_new(sizeof(Pair), TAG_PAIR);
    p->car = car;
    p->cdr = cdr;
    HEADER(p)->immutable = true;
    return (Value) p;
}

static inline Value list1_const(Value x)
{
    return cons_const(x, Qnil);
}

static inline Value list2_const(Value x, Value y)
{
    return cons_const(x, list1_const(y));
}

#define DUMMY_PAIR() ((Value) &(Pair) { .header = { .tag = TAG_PAIR }, \
                                        .car = Qundef, .cdr = Qnil })

#endif // INTERN_H
