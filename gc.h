#ifndef GC_H
#define GC_H

#include <setjmp.h>
#include <stdlib.h>

#include "utils.h"
#include "schaf.h"

#define VALUE_TAG(v) (*(ValueTag *)(v))

#define PAIR(v) ((Pair *) v)
#define STRING(v) ((String *) v)
#define PROCEDURE(v) ((Procedure *) v)
#define CFUNC(v) ((CFunc *) v)
#define CLOSURE(v) ((Closure *) v)
#define CONTINUATION(v) ((Continuation *) v)

typedef enum { // has the same values as Type
    TAG_PAIR  = TYPE_PAIR,
    TAG_STR   = TYPE_STR,
    TAG_CFUNC = TYPE_PROC + 1,
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
    Value env;
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

#define SCH_STACK_INIT(p) void *p; gc_stack_init(&p)

void gc_init(void);
void gc_add_root(const Value *r);
void gc_stack_init(const volatile void *b);
size_t gc_stack_get_size(const volatile void *sp);
void gc_set_print_stat(bool b);
void gc_set_init_size(size_t mib);

#endif
