#ifndef INTERN_H
#define INTERN_H

#include <setjmp.h>

#include "schaf.h"
#include "utils.h"

// shortcuts
typedef SchValue Value;
typedef
#ifdef __clang__
  volatile
#endif
  Value vValue; // workaround for clang
#define Qnil SCH_NULL
#define Qfalse SCH_FALSE
#define Qtrue SCH_TRUE
#define Qundef SCH_UNDEF

typedef uintptr_t Symbol;

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
    TYPE_PROMISE,
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
    TAG_PROMISE,
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
    Header header;
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
    const char *name;
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
    Value retval;
    void *sp;
    void *stack;
    size_t stack_len;
    jmp_buf state;
} Continuation;

typedef struct {
    Header header;
    Value *body;// use scary
} Vector;

typedef struct {
    Header header;
    Value parent;
    Table *table;
    const char *name;
} Env;

typedef enum {
    PORT_INPUT,
    PORT_OUTPUT
} PortType;

typedef struct {
    Header header;
    FILE *fp;
    PortType type;
    char *string;
} Port;

typedef struct {
    Header header;
    bool forced;
    Value env;
    Value val;
} Promise;

typedef struct {
    const char *func_name;
    Value loc; // maybe an Integer for resolution in caller
} StackFrame;

typedef struct {
    Header header;
    StackFrame **call_stack;
} Error;

#define HEADER(v) ((Header *) v)
#define VALUE_TAG(v) (HEADER(v)->tag)

#define INT(v) sch_integer_to_cint(v)
#define SYMBOL(v) sch_symbol_to_csymbol(v)
#define PAIR(v) ((Pair *) v)
#define LOCATED_PAIR(v) ((LocatedPair *) v)
#define STRING(v) (((String *) v)->body)
#define PROCEDURE(v) ((Procedure *) v)
#define CFUNC(v) ((CFunc *) v)
#define CLOSURE(v) ((Closure *) v)
#define CONTINUATION(v) ((Continuation *) v)
#define CFUNC_CLOSURE(v) ((CFuncClosure *) v)
#define VECTOR(v) (((Vector *) v)->body)
#define ENV(v) ((Env *) v)
#define PORT(v) ((Port *) v)
#define PROMISE(v) ((Promise *) v)
#define ERROR(v) (((Error *) ((v) & ~FLAG_MASK_ERROR))->call_stack)

typedef struct {
    int64_t *newline_pos; // list of position | int
    Value ast;
    char filename[];
} Source;

#pragma GCC visibility push(hidden) // also affects Clang

extern Value SYM_QUOTE, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING;
extern const uintptr_t FLAG_MASK_ERROR;

Source *iparse(FILE *in, const char *filename);
Value parse_datum(FILE *in, const char *filename);
void pos_to_line_col(int64_t pos, const int64_t *newline_pos, int64_t *line, int64_t *col);
[[gnu::format(printf, 1, 2)]]
void print_error_message(const char *fmt, ...);
[[gnu::format(printf, 1, 0)]]
void append_error_message_v(const char *fmt, va_list ap);
void *obj_new(ValueTag t, size_t size);
void source_free(Source *s);

void gc_init(const void *sp);
void gc_fin(void);
size_t gc_stack_get_size(const void *sp);
void gc_add_root(const Value *r);
ATTR_XMALLOC void *gc_malloc(size_t size);

bool sch_value_is_integer(Value v);
bool sch_value_is_symbol(Value v);
bool sch_value_is_string(Value v);
bool sch_value_is_pair(Value v);
Type sch_value_type_of(Value v);

int64_t sch_integer_to_cint(Value v);
const char *sch_symbol_to_cstr(Value v);
const char *sch_string_to_cstr(Value v);
Symbol sch_symbol_to_csymbol(Value v);
const char *sch_value_to_type_name(Value v);

Value sch_integer_new(int64_t i);
Value sch_symbol_new(const char *s);
Value sch_string_new(const char *s);

Value cons(Value car, Value cdr);
Value car(Value v);
Value cdr(Value v);
int64_t length(Value list);
Value list1(Value x);
Value list1_const(Value x);
Value list2_const(Value x, Value y);

Value vector_new(void);
Value vector_push(Value v, Value e);

#pragma GCC visibility pop

#define DUMMY_PAIR() ((Value) &(Pair) { \
            .header = { .tag = TAG_PAIR, .immutable = false }, \
            .car = Qundef, .cdr = Qnil \
        })
#define GET_SP(p) uintptr_t v##p = 0; void *volatile p = &v##p; UNPOISON(&p, sizeof(void *))

#endif // INTERN_H
