#ifndef INTERN_H
#define INTERN_H

#include <setjmp.h>

#include "schaf.h"
#include "utils.h"

// shortcuts
typedef SchValue Value;
#define Qnil SCH_NULL
#define Qfalse SCH_FALSE
#define Qtrue SCH_TRUE
#define Qundef SCH_UNDEF

typedef enum {
// immediate
    TYPE_BOOL,
    TYPE_INT,
    TYPE_SYMBOL,
    TYPE_NULL,
    TYPE_UNDEF,
// boxed (tagged)
    TYPE_PAIR,
    TYPE_STRING,
    TYPE_PROC,
    TYPE_VECTOR,
    TYPE_ENV,
    TYPE_PORT,
    TYPE_EOF,
} Type;

typedef enum {
    TAG_PAIR,
    TAG_STRING,
    TAG_CFUNC,
    TAG_SYNTAX, // almost a C Function
    TAG_CLOSURE,
    TAG_CONTINUATION,
    TAG_CFUNC_CLOSURE,
    TAG_VECTOR,
    TAG_ENV,
    TAG_PORT,
    TAG_EOF,
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

typedef struct {
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
    CFunc cfunc;
    Value data;
} CFuncClosure;

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
    Value *body;// use scary
} Vector;

typedef struct {
    Header header;
    char *name;
    Value parent;
    Table *table;
} Env;

typedef struct {
    Header header;
    FILE *fp;
    bool output;
    char *string;
    size_t string_size;
} Port;

typedef struct {
    const char *func_name;
    Value loc;
} StackFrame;

typedef struct {
    Header header;
    StackFrame **call_stack;
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
#define CFUNC_CLOSURE(v) ((CFuncClosure *) v)
#define VECTOR(v) ((Vector *) v)
#define ENV(v) ((Env *) v)
#define PORT(v) ((Port *) v)
#define ERROR(v) ((Error *) v)

typedef struct {
    char *filename;
    int64_t *newline_pos; // list of position | int
    Value ast;
} Source;

#pragma GCC visibility push(hidden) // also affects Clang

extern Value SYM_QUOTE, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING;

Source *iparse(FILE *in, const char *filename);
Value parse_datum(FILE *in, const char *filename);
void pos_to_line_col(int64_t pos, int64_t *newline_pos, int64_t *line, int64_t *col);
[[gnu::noreturn]] void raise_error(jmp_buf buf, const char *fmt, ...);
void *obj_new(size_t size, ValueTag t);
void source_free(Source *s);

void gc_init(uintptr_t *base_sp);
void gc_fin(void);

size_t gc_stack_get_size(uintptr_t *sp);
ATTR_XMALLOC void *gc_malloc(size_t size);

bool value_is_null(Value v);
bool value_is_int(Value v);
bool value_is_symbol(Value v);
bool value_is_string(Value v);
bool value_is_pair(Value v);
Type value_type_of(Value v);

int64_t value_to_int(Value v);
const char *value_to_string(Value v);
const char *value_to_type_name(Value v);

Value value_of_int(int64_t i);
Value value_of_symbol(const char *s);
Value value_of_string(const char *s);

Value cons(Value car, Value cdr);
Value car(Value v);
Value cdr(Value v);
int64_t length(Value list);

Value vector_new(void);
Value vector_push(Value v, Value e);

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

#define DUMMY_PAIR() ((Value) &(Pair) { \
            .header = { .tag = TAG_PAIR, .immutable = false }, \
            .car = Qundef, .cdr = Qnil \
        })

#endif // INTERN_H
