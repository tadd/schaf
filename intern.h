#ifndef INTERN_H
#define INTERN_H

#include <setjmp.h>

#include "schaf.h"
#include "table.h"
#include "utils.h"

typedef enum { // some have the same values as Type
    TAG_PAIR    = TYPE_PAIR,
    TAG_STR     = TYPE_STR,
    TAG_CFUNC   = TYPE_LAST + 1,
    TAG_SYNTAX, // almost a C Function
    TAG_CLOSURE,
    TAG_CONTINUATION,
} ValueTag;

typedef struct Pair {
    ValueTag tag; // common
    Value car, cdr;
} Pair;

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
    volatile void *sp;
    void *shelter;
    size_t shelter_len;
    Value call_stack;
    jmp_buf state;
    Value retval;
} Continuation;

#define PAIR(v) ((Pair *) v)
#define STRING(v) ((String *) v)
#define PROCEDURE(v) ((Procedure *) v)
#define CFUNC(v) ((CFunc *) v)
#define CLOSURE(v) ((Closure *) v)
#define CONTINUATION(v) ((Continuation *) v)

extern Value SYM_QUOTE, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING;

ATTR_HIDDEN Value append_at(Value last, Value elem);
ATTR_HIDDEN Value iparse(FILE *in, const char *filename);
ATTR_HIDDEN Value pair_to_id(Value p);
ATTR_HIDDEN void pos_to_line_col(int64_t pos, Value newline_pos, int64_t *line, int64_t *col);
ATTR_HIDDEN ATTR(noreturn) void raise_error(jmp_buf buf, const char *fmt, ...);
ATTR_HIDDEN Value reverse(Value l);

static inline Value list1(Value x)
{
    return cons(x, Qnil);
}

static inline Value list2(Value x, Value y)
{
    return cons(x, list1(y));
}

#endif // INTERN_H
