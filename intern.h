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
    ValueTag tag; // common
    Value car, cdr;
} Pair;

typedef struct {
    Pair pair;   // inherit
    int64_t pos; // value from ftell(3)
} LocatedPair;

typedef struct {
    ValueTag tag;
    char body[];
} String;

typedef struct {
    ValueTag tag;
    int64_t arity;
} Procedure;

typedef Value (*cfunc_t)(/*ANYARGS*/);
typedef struct {
    Procedure proc;
    cfunc_t cfunc;
} CFunc;

typedef struct {
    Procedure proc;
    Table *env;
    Value params;
    Value body;
} Closure;

typedef struct {
    Procedure proc;
    uintptr_t *sp;
    void *stack;
    size_t stack_len;
    Value call_stack;
    jmp_buf state;
    Value retval;
} Continuation;

#define PAIR(v) ((Pair *) v)
#define LOCATED_PAIR(v) ((LocatedPair *) v)
#define STRING(v) ((String *) v)
#define PROCEDURE(v) ((Procedure *) v)
#define CFUNC(v) ((CFunc *) v)
#define CLOSURE(v) ((Closure *) v)
#define CONTINUATION(v) ((Continuation *) v)

#pragma GCC visibility push(hidden) // also affects Clang

extern Value SYM_QUOTE, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING;

Value iparse(FILE *in, const char *filename);
void pos_to_line_col(int64_t pos, Value newline_pos, int64_t *line, int64_t *col);
ATTR(noreturn) void raise_error(jmp_buf buf, const char *fmt, ...);
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

static inline Value list2(Value x, Value y)
{
    return cons(x, list1(y));
}

#define DUMMY_PAIR() ((Value) &(Pair) { .tag = TAG_PAIR, .car = Qundef, .cdr = Qnil })

#endif // INTERN_H
