#ifndef INTERN_H
#define INTERN_H

#include <setjmp.h>

#include "schaf.h"
#include "utils.h"

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
    TYPE_ENV,
    TYPE_PORT,
} Type;

typedef enum {
    TAG_PAIR,
    TAG_STRING,
    TAG_ESTRING,
    TAG_CFUNC,
    TAG_SYNTAX, // almost a C Function
    TAG_CLOSURE,
    TAG_CONTINUATION,
    TAG_ENV,
    TAG_PORT,
    // internal use only
    TAG_PARSER,
    TAG_ERROR,
    TAG_LAST = TAG_ERROR
} ValueTag;

typedef struct {
    ValueTag tag;
    bool immutable;
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
    void *stack;
    size_t stack_len;
    jmp_buf regs;
} ExecutionState;

typedef struct {
    Procedure proc;
    uintptr_t *sp;
    Value retval;
    ExecutionState *exstate;
} Continuation;

typedef enum {
    TOK_TYPE_LPAREN,
    TOK_TYPE_RPAREN,
    TOK_TYPE_QUOTE,
    TOK_TYPE_GRAVE,
    TOK_TYPE_COMMA,
    TOK_TYPE_SPLICE,
    TOK_TYPE_INT,
    TOK_TYPE_DOT,
    TOK_TYPE_STRING,
    TOK_TYPE_IDENT,
    TOK_TYPE_CONST,
    TOK_TYPE_EOF
} TokenType;

typedef struct {
    TokenType type;
    Value value;
} Token;

typedef struct {
    FILE *in;
    const char *filename;
    Token *prev_token;
    Value newline_pos; // list of pos | int
} Parser;

typedef struct {
    char *name;
    Value parent;
    Table *table;
} Env;

typedef struct {
    Header header;
    FILE *fp;
} Port;

typedef struct {
    Header header;
    Value call_stack; // list of '(func-name . location)
} Error;

enum {
    EMBED_LEN = sizeof(Continuation), // maybe the largest
};

typedef struct {
    Header header; // common
    union {
        char *string;
        char estring[EMBED_LEN];
        Pair pair;
        LocatedPair lpair;
        Procedure proc;
        CFunc cfunc;
        Closure closure;
        Continuation continuation;
        Parser parser;
        Env env;
        Port port;
        Error error;
    };
} SchObject;

#define OBJ(v) ((SchObject *) v)
#define HEADER(v) (&OBJ(v)->header)
#define VALUE_TAG(v) (HEADER(v)->tag)

#define PAIR(v) (&OBJ(v)->pair)
#define LOCATED_PAIR(v) (&OBJ(v)->lpair)
#define STRING(v) (LIKELY(VALUE_TAG(v) == TAG_ESTRING) ? OBJ(v)->estring : OBJ(v)->string)
#define PROCEDURE(v) (&OBJ(v)->proc)
#define CFUNC(v) (&OBJ(v)->cfunc)
#define CLOSURE(v) (&OBJ(v)->closure)
#define CONTINUATION(v) (&OBJ(v)->continuation)
#define PARSER(v) (&OBJ(v)->parser)
#define ENV(v) (&OBJ(v)->env)
#define PORT(v) (&OBJ(v)->port)
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
            .header = { .tag = TAG_PAIR, .immutable = false }, \
            .pair = { .car = Qundef, .cdr = Qnil } \
        })

#endif // INTERN_H
