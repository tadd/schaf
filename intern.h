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
    TAG_HSTRING,
    TAG_ESTRING,
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
    TAG_CHUNK,  // not allocated
    TAG_LAST = TAG_ERROR
} ValueTag;

typedef struct {
    ValueTag tag;
    bool immutable;
    bool living; // used in GC
    size_t size;
} Header;

typedef struct {
    Value car, cdr;
} Pair;

typedef struct {
    Pair pair;   // inherit
    int64_t pos; // value from ftell(3)
} LocatedPair;

typedef struct {
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
    uintptr_t *volatile sp;
    void *volatile stack;
    size_t stack_len;
    jmp_buf regs;
} ExecutionState;

typedef struct {
    Procedure proc;
    Value retval;
    ExecutionState *exstate;
} Continuation;

typedef struct {
    char *name;
    Value parent;
    Table *table;
} Env;

typedef enum {
    PORT_INPUT,
    PORT_OUTPUT
} PortType;

typedef struct {
    FILE *fp;
    PortType type;
    char *string;
} Port;

typedef struct {
    const char *func_name;
    Value loc;
} StackFrame;

typedef struct {
    Header header; // common
    union {
        alignas(16) Header *next;
        char *hstring;
        char estring[sizeof(Closure)];// may be the largest
        Pair pair;
        LocatedPair lpair;
        Procedure proc;
        CFunc cfunc;
        Closure closure;
        Continuation continuation;
        CFuncClosure cfunc_closure;
        Value *vector;// use scary
        Env env;
        Port port;
        StackFrame **error;// ditto
    };
} SchObject;

#define OBJ(v) ((SchObject *) v)
#define HEADER(v) (&OBJ(v)->header)
#define VALUE_TAG(v) (HEADER(v)->tag)

#define HEADER_NEXT(v) (OBJ(v)->next)
#define PAIR(v) (&OBJ(v)->pair)
#define LOCATED_PAIR(v) (&OBJ(v)->lpair)
#define ESTRING(v) (OBJ(v)->estring)
#define HSTRING(v) (OBJ(v)->hstring)
#define STRING(v) (VALUE_TAG(v) == TAG_HSTRING ? HSTRING(v) : ESTRING(v))
#define PROCEDURE(v) (&OBJ(v)->proc)
#define CFUNC(v) (&OBJ(v)->cfunc)
#define CLOSURE(v) (&OBJ(v)->closure)
#define CONTINUATION(v) (&OBJ(v)->continuation)
#define CFUNC_CLOSURE(v) (&OBJ(v)->cfunc_closure)
#define VECTOR(v) (OBJ(v)->vector)
#define ENV(v) (&OBJ(v)->env)
#define PORT(v) (&OBJ(v)->port)
#define ERROR(v) (OBJ(v)->error)

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
SchObject *obj_new(ValueTag t);
void source_free(Source *s);

void gc_init(uintptr_t *volatile base_sp);
void gc_fin(void);
void gc_add_root(const Value *r);
size_t gc_stack_get_size(uintptr_t *volatile sp);
ATTR_XMALLOC Header *gc_malloc(size_t size);

bool sch_value_is_integer(Value v);
bool sch_value_is_symbol(Value v);
bool sch_value_is_string(Value v);
bool sch_value_is_pair(Value v);
Type sch_value_type_of(Value v);

int64_t sch_integer_to_cint(Value v);
const char *sch_symbol_to_cstr(Value v);
const char *sch_string_to_cstr(Value v);
const char *sch_value_to_type_name(Value v);

Value sch_integer_new(int64_t i);
Value sch_symbol_new(const char *s);
Value sch_string_new(const char *s);

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
    SchObject *o = obj_new(TAG_PAIR);
    HEADER(o)->immutable = true;
    Pair *p = PAIR(o);
    p->car = car;
    p->cdr = cdr;
    return (Value) o;
}

static inline Value list1_const(Value x)
{
    return cons_const(x, Qnil);
}

static inline Value list2_const(Value x, Value y)
{
    return cons_const(x, list1_const(y));
}

#define DUMMY_PAIR() ((Value) &(SchObject) { \
            .header = { .tag = TAG_PAIR, .immutable = false, .living = false }, \
            .pair = { .car = Qundef, .cdr = Qnil } \
        })
#define GET_SP(p) uintptr_t v##p = 0, *volatile p = &v##p; UNPOISON(&p, sizeof(uintptr_t *))

#endif // INTERN_H
