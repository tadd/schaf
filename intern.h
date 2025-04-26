#ifndef INTERN_H
#define INTERN_H

#include <setjmp.h>

#include "schaf.h"
#include "utils.h"

typedef enum {
    TAG_CHUNK,  // not allocated
    TAG_PAIR,
    TAG_STRING,
    TAG_CFUNC,
    TAG_SYNTAX, // almost a C Function
    TAG_CLOSURE,
    TAG_CONTINUATION,
    TAG_USER_OBJ,
    TAG_LAST = TAG_USER_OBJ
} ValueTag;

typedef struct Header {
    ValueTag tag;
    size_t size;
    struct Header *next;
    bool living; // used in GC
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
} Procedure;

typedef struct {
    Procedure proc;
    char *name;
    union {
        void *cfunc;
        Value (*f0)(Table *);
        Value (*f1)(Table *, Value);
        Value (*f2)(Table *, Value, Value);
        Value (*f3)(Table *, Value, Value, Value);
    };
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

typedef struct {
    Header header;
    char *name;
    void *obj;
    void (*mark)(void *p);
    void (*free)(void *p);
} UserObject;

#define HEADER(v) ((Header*)(v))
#define VALUE_TAG(v) (HEADER(v)->tag)

#define PAIR(v) ((Pair *) v)
#define LOCATED_PAIR(v) ((LocatedPair *) v)
#define STRING(v) ((String *) v)
#define PROCEDURE(v) ((Procedure *) v)
#define CFUNC(v) ((CFunc *) v)
#define CLOSURE(v) ((Closure *) v)
#define CONTINUATION(v) ((Continuation *) v)
#define USER_OBJ(v) ((UserObject *) (v))

#pragma GCC visibility push(hidden) // also affects Clang

extern Value SYM_QUOTE, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING;

Value iparse(FILE *in, const char *filename);
void pos_to_line_col(int64_t pos, Value newline_pos, int64_t *line, int64_t *col);
ATTR(noreturn) void raise_error(jmp_buf buf, const char *fmt, ...);
Value reverse(Value l);
void *obj_new(size_t size, ValueTag t);
bool value_is_immediate(Value v);

void gc_init(uintptr_t *base_sp);
void gc_fin(void);

void gc_add_root(const Value *r);
void gc_add_root_env(Table **env);
size_t gc_stack_get_size(uintptr_t *sp);
ATTR_XMALLOC Header *gc_malloc(size_t size);
void env_mark(void *env);
void env_free(void *env);

#pragma GCC visibility pop

static inline Value list1(Value x)
{
    return cons(x, Qnil);
}

static inline Value list2(Value x, Value y)
{
    return cons(x, list1(y));
}

#define DUMMY_PAIR() ((Value) &(Pair) { .header = { .tag = TAG_PAIR, .living = false }, \
                                        .car = Qundef, .cdr = Qnil })

#endif // INTERN_H
