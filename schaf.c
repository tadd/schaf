#include <inttypes.h>
#include <math.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <libgen.h>
#include <limits.h>
#include <unistd.h>

#include "schaf.h"
#include "intern.h"
#include "utils.h"

//
// Types
//

static const char *TYPE_NAMES[] = {
    [TYPE_NULL] = "null",
    [TYPE_BOOL] = "boolean",
    [TYPE_INT] = "integer",
    [TYPE_SYMBOL] = "symbol",
    [TYPE_UNDEF] = "undef",
    [TYPE_PAIR] = "pair",
    [TYPE_STRING] = "string",
    [TYPE_PROC] = "procedure",
};

#define VALUE_TAG(v) (*(ValueTag*)(v))
#define OF_BOOL(v) ((v) ? Qtrue : Qfalse)

// Value (uintptr_t):
//   0b.....000 Pointer (Unchangeable pattern!)
//   0b.......1 Integer
//   0b......10 Symbol
//   0b0--00100 #f
//   0b0--01100 #t
//   0b0-010100 <undef>
typedef const uintptr_t Flag;
static Flag FLAG_NBIT_SYM = 2;
static Flag FLAG_NBIT_INT = 1;
static Flag FLAG_MASK     = 0b111; // for 64 bit machine
static Flag FLAG_MASK_SYM =  0b11;
static Flag FLAG_MASK_INT =   0b1;
static Flag FLAG_SYM      =  0b10;
static Flag FLAG_INT      =   0b1;
const Value Qnil   = 0b11100U;
const Value Qfalse = 0b00100U;
const Value Qtrue  = 0b01100U;
const Value Qundef = 0b10100U; // may be an error or something

static const int64_t CFUNCARG_MAX = 3;

//
// Runtime-locals (aka global variables)
//

// Environment: list of Frames
// Frame: Table of 'symbol => <value>
static Table *toplevel_environment;
static Value symbol_names = Qnil; // ("name0" "name1" ...)
Value SYM_ELSE, SYM_QUOTE, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING,
    SYM_RARROW;
static const char *load_basedir = NULL;
static Value call_stack = Qnil; // list of pairs
static const char *curr_cfunc_name;
static Value source_data = Qnil; // (a)list of AST: (filename syntax_list newline_positions)
// newline_positions: list of pos | int

//
// value_is_*: Type Checks
//

inline bool value_is_int(Value v)
{
    return v & FLAG_MASK_INT;
}

inline bool value_is_symbol(Value v)
{
    return (v & FLAG_MASK_SYM) == FLAG_SYM;
}

static bool value_is_immediate(Value v)
{
    return v & FLAG_MASK;
}

static inline bool value_tag_is(Value v, ValueTag expected)
{
    return !value_is_immediate(v) && VALUE_TAG(v) == expected;
}

inline bool value_is_string(Value v)
{
    return value_tag_is(v, TAG_STRING);
}

static inline bool value_is_procedure(Value v)
{
    if (value_is_immediate(v))
        return false;
    switch (VALUE_TAG(v)) {
    case TAG_SYNTAX:
    case TAG_CFUNC:
    case TAG_CLOSURE:
    case TAG_CONTINUATION:
        return true;
    case TAG_STRING:
    case TAG_PAIR:
        return false;
    }
    UNREACHABLE();
}

inline bool value_is_pair(Value v)
{
    return value_tag_is(v, TAG_PAIR);
}

static Type immediate_type_of(Value v)
{
    if (v == Qnil)
        return TYPE_NULL;
    if (value_is_int(v))
        return TYPE_INT;
    if (value_is_symbol(v))
        return TYPE_SYMBOL;
    if (v == Qtrue || v == Qfalse)
        return TYPE_BOOL;
    if (v == Qundef)
        return TYPE_UNDEF;
    UNREACHABLE();
}

Type value_type_of(Value v)
{
    if (value_is_immediate(v))
        return immediate_type_of(v);
    switch (VALUE_TAG(v)) {
    case TAG_STRING:
        return TYPE_STRING;
    case TAG_PAIR:
        return TYPE_PAIR;
    case TAG_CFUNC:
    case TAG_SYNTAX:
    case TAG_CLOSURE:
    case TAG_CONTINUATION:
        return TYPE_PROC;
    }
    UNREACHABLE();
}

static inline const char *value_type_to_string(Type t)
{
    return TYPE_NAMES[t];
}

const char *value_to_type_name(Value v)
{
    Type t = value_type_of(v);
    return value_type_to_string(t);
}

// value_to_*: Convert internal data to external plain C

inline int64_t value_to_int(Value x)
{
#if __x86_64__
    return (int64_t) x >> FLAG_NBIT_INT;
#else
    int64_t i = x;
    return (i - 1) / (1 << FLAG_NBIT_INT);
#endif
}

inline Symbol value_to_symbol(Value v)
{
    return (Symbol) v >> FLAG_NBIT_SYM;
}

static const char *name_nth(Value list, int64_t n)
{
    for (int64_t i = 0; i < n; i++) {
        list = cdr(list);
        if (list == Qnil)
            return NULL;
    }
    Value name = car(list);
    return STRING(name)->body;
}

static const char *unintern(Symbol sym)
{
    const char *name = name_nth(symbol_names, (int64_t) sym);
    if (name == NULL) // fatal; every known symbols should have a name
        error("symbol %lu not found", sym);
    return name;
}

inline const char *value_to_string(Value v)
{
    if (value_is_symbol(v))
        return unintern(value_to_symbol(v));
    return STRING(v)->body;
}

// value_of_*: Convert external plain C data to internal

inline Value value_of_int(int64_t i)
{
    Value v = i;
    return v << FLAG_NBIT_INT | FLAG_INT;
}

static Symbol intern(const char *name)
{
    Value last = Qnil;
    int64_t i = 0;
    // find
    for (Value p = symbol_names; p != Qnil; last = p, p = cdr(p)) {
        Value v = car(p);
        if (strcmp(STRING(v)->body, name) == 0)
            return i;
        i++;
    }
    // or put at `i`
    Value s = value_of_string(name);
    Value next = list1(s);
    if (last == Qnil)
        symbol_names = next;
    else
        PAIR(last)->cdr = next;
    return i;
}

inline Value value_of_symbol(const char *s)
{
    Symbol sym = intern(s);
    return (Value) (sym << FLAG_NBIT_SYM | FLAG_SYM);
}

void *obj_new(size_t size, ValueTag t)
{
    void *p = gc_malloc(size);
    VALUE_TAG(p) = t;
    return p;
}

Value value_of_string(const char *s)
{
    size_t len = strlen(s) + 1;
    String *str = obj_new(sizeof(String) + len, TAG_STRING);
    strcpy(str->body, s);
    return (Value) str;
}

static void expect_cfunc_arity(int64_t actual)
{
    if (actual <= CFUNCARG_MAX)
        return;
    error("arity too large: expected ..%"PRId64" but got %"PRId64,
          CFUNCARG_MAX, actual);
}

static Value value_of_cfunc(const char *name, cfunc_t cfunc, int64_t arity)
{
    expect_cfunc_arity(arity);
    CFunc *f = obj_new(sizeof(CFunc), TAG_CFUNC);
    f->name = xstrdup(name);
    f->proc.arity = arity;
    f->cfunc = cfunc;
    return (Value) f;
}

static Value value_of_syntax(const char *name, cfunc_t cfunc, int64_t arity)
{
    Value sp = value_of_cfunc(name, cfunc, arity);
    VALUE_TAG(sp) = TAG_SYNTAX;
    return sp;
}

static Value value_of_closure(Table *env, Value params, Value body)
{
    Closure *f = obj_new(sizeof(Closure), TAG_CLOSURE);
    f->proc.arity = (params == Qnil || value_is_pair(params)) ? length(params) : -1;
    f->env = env;
    f->params = params;
    f->body = body;
    return (Value) f;
}

// and `cons` is well-known name than "value_of_pair"

//
// Errors
//

#define error(fmt, ...) \
    error("%s:%d of %s: " fmt, __FILE__, __LINE__, __func__ __VA_OPT__(,) __VA_ARGS__)

static jmp_buf jmp_runtime_error, jmp_exit;
static uint8_t exit_status; // to be portable
static char errmsg[BUFSIZ];

ATTR(noreturn)
void raise_error(jmp_buf buf, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(errmsg, sizeof(errmsg), fmt, ap);
    va_end(ap);
    longjmp(buf, 1);
}

ATTR(noreturn)
void runtime_error(const char *fmt, ...)
{
    size_t nprep = 0;
    if (curr_cfunc_name != NULL) {
        nprep = strlen(curr_cfunc_name) + 2; // strlen(": ")
        snprintf(errmsg, sizeof(errmsg), "%s: ", curr_cfunc_name);
    }
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(errmsg + nprep, sizeof(errmsg) - nprep, fmt, ap);
    va_end(ap);
    longjmp(jmp_runtime_error, 1);
}

const char *error_message(void)
{
    return errmsg;
}

static void expect_type(Type expected, Value v)
{
    Type t = value_type_of(v);
    if (t == expected)
        return;
    runtime_error("type expected %s but got %s",
                  value_type_to_string(expected), value_type_to_string(t));
}
#define expect_type_twin(t, x, y) expect_type(t, x), expect_type(t, y)

static void expect_type_or(Type e1, Type e2, Value v)
{
    Type t = value_type_of(v);
    if (t == e1 || t == e2)
        return;
    runtime_error("type expected %s or %s but got %s",
                  value_type_to_string(e1), value_type_to_string(e2),
                  value_type_to_string(t));
}

static void expect_list_head(Value v)
{
    expect_type_or(TYPE_NULL, TYPE_PAIR, v);
}

static void expect_arity_range(int64_t min, int64_t max, Value args)
{
    int64_t actual = length(args);
    if (min <= actual && (max == -1 || actual <= max))
        return;
    runtime_error("wrong number of arguments: expected %"PRId64"..%"PRId64" but got %"PRId64,
                  min, max, actual);
}

static void expect_arity_min(int64_t min, Value args)
{
    expect_arity_range(min, -1, args);
}

static void expect_arity(int64_t expected, Value args)
{
    int64_t actual = length(args);
    if (expected < 0 || expected == actual)
        return;
    runtime_error("wrong number of arguments: expected %"PRId64" but got %"PRId64,
                  expected, actual);
}

static Value apply_cfunc(Table *env, Value proc, Value args)
{
    Value a[CFUNCARG_MAX];
    CFunc *cf = CFUNC(proc);
    int64_t n = cf->proc.arity;
    Value arg = args;
    for (int i = 0; i < n; i++) {
        a[i] = car(arg);
        arg = cdr(arg);
    }
    cfunc_t f = cf->cfunc;
    curr_cfunc_name = cf->name;
#if defined(__clang__) && __clang_major__ >= 15
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-non-prototype"
#endif
    switch (n) {
    case -1:
        return (*f)(env, args);
    case 0:
        return (*f)(env);
    case 1:
        return (*f)(env, a[0]);
    case 2:
        return (*f)(env, a[0], a[1]);
    case 3:
        return (*f)(env, a[0], a[1], a[2]);
    default:
        error("arity too large: %"PRId64, n);
    }
#if defined(__clang__) && __clang_major__ >= 15
#pragma clang diagnostic pop
#endif
}

static Value append2(Value l1, Value l2)
{
    if (l2 == Qnil)
        return l1;
    if (l1 == Qnil)
        return l2;

    Value ret = list1(car(l1)), prev = ret;
    for (Value p = cdr(l1); p != Qnil; p = cdr(p)) {
        Value curr = list1(car(p));
        PAIR(prev)->cdr = curr;
        prev = curr;
    }
    PAIR(prev)->cdr = l2;
    return ret;
}

static Table *env_put(Table *env, Value sym, Value val)
{
    return table_put(env, value_to_symbol(sym), val);
}

static bool env_set(Table *env, Value sym, Value val)
{
    return table_set(env, value_to_symbol(sym), val);
}

static Value env_get(const Table *env, Value sym)
{
    Value found = table_get(env, value_to_symbol(sym));
    if (found == TABLE_NOT_FOUND)
        return Qundef;
    return found;
}

static Value eval_body(Table *env, Value body);

//PTR
static Value apply_closure(Value proc, Value args)
{
    Closure *cl = CLOSURE(proc);
    int64_t arity = cl->proc.arity;
    Table *clenv = table_inherit(cl->env);
    Value params = cl->params;
    if (arity == -1)
        env_put(clenv, params, args);
    else {
        for (Value pa = args, pp = params; pa != Qnil; pa = cdr(pa), pp = cdr(pp))
            env_put(clenv, car(pp), car(pa));
    }
    return eval_body(clenv, cl->body); // XXX: table_free(clenv)?
}

ATTR(noreturn) ATTR(noinline)
static void jump(Continuation *cont)
{
    call_stack = cont->call_stack;
    memcpy(cont->sp, cont->stack, cont->stack_len);
    longjmp(cont->state, 1);
}

#define GET_SP(p) uintptr_t v##p = 0, *p = &v##p; UNPOISON(&p, sizeof(uintptr_t *))

ATTR(noreturn) ATTR(noinline)
static void apply_continuation(Value f, Value args)
{
    GET_SP(sp);
    Continuation *cont = CONTINUATION(f);
    cont->retval = car(args);
    int64_t d = sp - cont->sp;
    if (d < 1)
        d = 1;
    volatile uintptr_t pad[d];
    pad[0] = pad[d-1] = 0; // avoid unused
    jump(cont);
}

// expects proc and args have been evaluated if necessary
static Value apply(Table *env, Value proc, Value args)
{
    expect_arity(PROCEDURE(proc)->arity, args);
    switch (VALUE_TAG(proc)) {
    case TAG_SYNTAX:
    case TAG_CFUNC:
        return apply_cfunc(env, proc, args);
    case TAG_CLOSURE:
        return apply_closure(proc, args);
    case TAG_CONTINUATION:
        apply_continuation(proc, args); // no return!
    default:
        UNREACHABLE();
    }
}

// Note: Do not mistake this for "(define-syntax ...)" which related to macros
static void define_syntax(Table *env, const char *name, cfunc_t cfunc, int64_t arity)
{
    env_put(env, value_of_symbol(name), value_of_syntax(name, cfunc, arity));
}

static void define_procedure(Table *env, const char *name, cfunc_t cfunc, int64_t arity)
{
    env_put(env, value_of_symbol(name), value_of_cfunc(name, cfunc, arity));
}

static Value assq(Value key, Value l);

#define CXR1(f, x) f(a, x); f(d, x);
#define CXR2(f, x) CXR1(f, a ## x) CXR1(f, d ## x)
#define CXR3(f, x) CXR2(f, a ## x) CXR2(f, d ## x)
#define CXR4(f, x) CXR3(f, a) CXR3(f, d)
#define CXRS(f) CXR2(f,) CXR3(f,) CXR4(f,)

#define DEF_CXR(x, y) \
    static Value c##x##y##r(Value v) { return c##x##r(c##y##r(v)); }
CXRS(DEF_CXR)

//
// Evaluation
//
static Value eval(Table *env, Value v);

static Value eval_body(Table *env, Value body)
{
    Value last = Qnil;
    for (Value p = body; p != Qnil; p = cdr(p))
        last = eval(env, car(p));
    return last;
}

static Value eval_body_tmpenv(Table *env, Value body)
{
    Value ret = eval_body(env, body);
    table_free(env);
    return ret;
}

static Value map_eval(Table *env, Value l)
{
    Value mapped = DUMMY_PAIR();
    for (Value last = mapped, p = l; p != Qnil; p = cdr(p))
        last = PAIR(last)->cdr = list1(eval(env, car(p)));
    return cdr(mapped);
}

static void call_stack_push(Value l)
{
    call_stack = cons(l, call_stack);
}

static void call_stack_pop(void)
{
    expect_type(TYPE_PAIR, call_stack);
    call_stack = cdr(call_stack);
}

static Value eval_apply(Table *env, Value l)
{
    call_stack_push(l);
    Value symproc = car(l), args = cdr(l);
    Value proc = eval(env, symproc);
    expect_type(TYPE_PROC, proc);
    if (!value_tag_is(proc, TAG_SYNTAX))
        args = map_eval(env, args);
    const char *prev_name = curr_cfunc_name;
    curr_cfunc_name = NULL;
    Value ret = apply(env, proc, args);
    curr_cfunc_name = prev_name;
    call_stack_pop();
    return ret;
}

static Value lookup_or_error(Table *env, Value v)
{
    Value p = env_get(env, v);
    if (p == Qundef)
        runtime_error("unbound variable: %s", value_to_string(v));
    return p;
}

static Value eval(Table *env, Value v)
{
    if (value_is_symbol(v))
        return lookup_or_error(env, v);
    if (v == Qnil || !value_is_pair(v))
        return v;
    return eval_apply(env, v);
}

ATTR(format(printf, 1, 2))
static int append_error_message(const char *fmt, ...)
{
    size_t len = strlen(errmsg);
    va_list ap;
    va_start(ap, fmt);
    int n = vsnprintf(errmsg + len, sizeof(errmsg) - len, fmt, ap);
    va_end(ap);
    return n;
}

static void dump_line_column(Value vfilename, int64_t pos)
{
    int64_t line, col;
    Value data = assq(vfilename, source_data);
    if (data == Qnil) {
        append_error_message("\n\t<unknown>");
        return;
    }
    Value newline_pos = caddr(data);
    pos_to_line_col(pos, newline_pos, &line, &col);
    const char *filename = value_to_string(vfilename);
    append_error_message("\n\t%s:%"PRId64":%"PRId64" in ", filename, line, col);
}

static bool find_pair(Value tree, Value pair)
{
    for (Value p = tree; p != Qnil; p = cdr(p)) {
        if (p == pair)
            return true;
        if (!value_is_pair(p))
            return false;
        Value v = car(p);
        if (value_is_pair(v) && find_pair(v, pair))
            return true;
    }
    return false;
}

static Value find_filename(Value pair)
{
    for (Value p = source_data; p != Qnil; p = cdr(p)) {
        Value tree = cadar(p);
        if (find_pair(tree, pair))
            return caar(p); // filename
    }
    return Qfalse;
}

static void dump_callee_name(Value callers)
{
    if (callers == Qnil) {
        append_error_message("<toplevel>");
        return;
    }
    Value sym = caar(callers);
    if (!value_is_symbol(sym)) {
        append_error_message("<unknown>");
        return;
    }
    const char *name = value_to_string(sym);
    append_error_message("'%s'", name);
}

static void dump_frame(Value pair, Value callers)
{
    Value filename = find_filename(pair);
    if (filename == Qfalse) {
        append_error_message("\n\t<unknown>");
        return;
    }
    dump_line_column(filename, LOCATED_PAIR(pair)->pos);
    dump_callee_name(callers);
}

static void dump_stack_trace()
{
    for (Value p = call_stack, next; p != Qnil; p = next) {
        next = cdr(p);
        dump_frame(car(p), next);
    }
}

static void fdisplay(FILE* f, Value v);

static void call_stack_check_consistency(void)
{
    if (call_stack == Qnil)
        return;
    fprintf(stderr, "BUG: got non-null call stack: ");
    fdisplay(stderr, call_stack);
    fprintf(stderr, "\n");
}

static Value iload(FILE *in, const char *filename)
{
    Value ast = iparse(in, filename), l = cadr(ast);
    if (l == Qundef)
        return Qundef;
    source_data = cons(ast, source_data);
    if (setjmp(jmp_runtime_error) != 0) {
        dump_stack_trace();
        return Qundef;
    }
    if (setjmp(jmp_exit) != 0)
        return value_of_int(exit_status);
    call_stack = Qnil;
    Value ret = eval_body(toplevel_environment, l);
    call_stack_check_consistency();
    return ret;
}

static Value iload_inner(FILE *in, const char *path)
{
    Value ast = iparse(in, path), l = cadr(ast);
    source_data = cons(ast, source_data);
    if (l == Qundef)
        return Qundef;
    return eval_body(toplevel_environment, l);
}

Value eval_string(const char *in)
{
    FILE *f = fmemopen((char *) in, strlen(in), "r");
    Value v = iload(f, "<inline>");
    fclose(f);
    return v;
}

static FILE *open_loadable(const char *path)
{
    static char rpath[PATH_MAX];
    char joined[PATH_MAX];
    snprintf(joined, sizeof(joined), "%s/%s", load_basedir, path);
    (void)!realpath(joined, rpath);

    FILE *in = fopen(rpath, "r");
    if (in == NULL)
        return NULL;
    load_basedir = dirname(rpath);
    return in;
}

Value load(const char *path)
{
    FILE *in = open_loadable(path);
    if (in == NULL)
        error("can't open file: %s", path);
    Value retval = iload(in, path);
    fclose(in);
    return retval;
}

static Value load_inner(const char *path)
{
    const char *basedir_saved = load_basedir;
    FILE *in = open_loadable(path);
    if (in == NULL)
        runtime_error("load: can't open file: %s", path);
    Value retval = iload_inner(in, path);
    fclose(in);
    load_basedir = basedir_saved;
    return retval;
}

//
// Built-in Procedures / Syntax
//

#define UNUSED ATTR(unused)
// 4.1. Primitive expression types
// 4.1.2. Literal expressions
static Value syn_quote(UNUSED Table *env, Value datum)
{
    return datum;
}

// 4.1.4. Procedures
//PTR -- proper tail recursion needed
static Value syn_lambda(Table *env, Value args)
{
    Value params = car(args), body = cdr(args);
    if (params != Qnil)
        expect_type_or(TYPE_PAIR, TYPE_SYMBOL, params);
    expect_type(TYPE_PAIR, body);
    return value_of_closure(env, params, body);
}

// 4.1.5. Conditionals
//PTR
static Value syn_if(Table *env, Value args)
{
    expect_arity_range(2, 3, args);

    Value cond = car(args), then = cadr(args);
    if (eval(env, cond) != Qfalse)
        return eval(env, then);
    Value els = cddr(args);
    if (els == Qnil)
        return Qnil;
    return eval(env, car(els));
}

// 4.1.6. Assignments
static Value iset(Table *env, Value ident, Value val)
{
    bool found = env_set(env, ident, val);
    if (!found)
        runtime_error("unbound variable: %s", value_to_string(ident));
    return Qnil;
}

static Value syn_set(Table *env, Value ident, Value expr)
{
    expect_type(TYPE_SYMBOL, ident);
    return iset(env, ident, eval(env, expr));
}

// 4.2. Derived expression types
// 4.2.1. Conditionals
static Value cond_eval_recipient(Table *env, Value test, Value recipients)
{
    expect_type(TYPE_PAIR, recipients);
    Value recipient = eval(env, car(recipients)), rest = cdr(recipients);
    expect_type(TYPE_PROC, recipient);
    if (rest != Qnil)
        runtime_error("only one expression expected after =>");
    return apply(env, recipient, list1(test));
}

//PTR
static Value syn_cond(Table *env, Value clauses)
{
    expect_arity_min(1, clauses);

    for (Value p = clauses; p != Qnil; p = cdr(p)) {
        Value clause = car(p);
        expect_type(TYPE_PAIR, clause);
        Value test = car(clause);
        Value exprs = cdr(clause);
        if (test == SYM_ELSE)
            return eval_body(env, exprs);
        Value t = eval(env, test);
        if (t != Qfalse) {
            if (exprs == Qnil)
                return t;
            if (car(exprs) == SYM_RARROW)
                return cond_eval_recipient(env, t, cdr(exprs));
            return eval_body(env, exprs);
        }
    }
    return Qnil;
}

static Value memq(Value key, Value l);

//PTR
static Value syn_case(Table *env, Value args)
{
    expect_arity_min(2, args);
    Value key = eval(env, car(args)), clauses = cdr(args);
    expect_list_head(clauses);

    for (Value p = clauses; p != Qnil; p = cdr(p)) {
        Value clause = car(p);
        expect_type(TYPE_PAIR, clause);
        Value data = car(clause), exprs = cdr(clause);
        expect_type(TYPE_PAIR, exprs);
        if (data == SYM_ELSE || memq(key, data) != Qfalse)
            return eval_body(env, exprs);
    }
    return Qnil;
}

//PTR
static Value syn_and(Table *env, Value args)
{
    Value last = Qtrue;
    for (Value p = args; p != Qnil; p = cdr(p)) {
        if ((last = eval(env, car(p))) == Qfalse)
            break;
    }
    return last;
}

//PTR
static Value syn_or(Table *env, Value args)
{
    Value last = Qfalse;
    for (Value p = args; p != Qnil; p = cdr(p)) {
        if ((last = eval(env, car(p))) != Qfalse)
            break;
    }
    return last;
}

// 4.2.2. Binding constructs
static void transpose_2xn(Value ls, Value *pfirsts, Value *pseconds) // 2 * n
{
    Value firsts = DUMMY_PAIR(), seconds = DUMMY_PAIR();
    for (Value lfirsts = firsts, lseconds = seconds; ls != Qnil; ls = cdr(ls)) {
        Value l = car(ls);
        expect_arity(2, l);
        lfirsts = PAIR(lfirsts)->cdr = list1(car(l));
        lseconds = PAIR(lseconds)->cdr = list1(cadr(l));
    }
    *pfirsts = cdr(firsts);
    *pseconds = cdr(seconds);
}

static Value let(Table *env, Value var, Value bindings, Value body)
{
    expect_list_head(bindings);
    Value params = Qnil, symargs = Qnil;
    transpose_2xn(bindings, &params, &symargs);
    Value args = map_eval(env, symargs);
    if (var == Qfalse) {
        Value proc = value_of_closure(env, params, body);
        return apply_closure(proc, args);
    }
    Table *letenv = table_inherit(env);
    Value proc = value_of_closure(letenv, params, body);
    env_put(letenv, var, proc); // affects as proc->env
    Value ret = apply_closure(proc, args);
    table_free(letenv);
    return ret;
}

//PTR
static Value syn_let(Table *env, Value args)
{
    expect_arity_min(2, args);
    Value bind_or_var = car(args), body = cdr(args);
    Value var = Qfalse, bindings = bind_or_var;
    if (value_is_symbol(bind_or_var)) {
        var = bind_or_var;
        bindings = car(body);
        body = cdr(body);
    }
    return let(env, var, bindings, body);
}

static Value let_star(Table *env, Value bindings, Value body)
{
    expect_list_head(bindings);
    Table *letenv = env;
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        Value b = car(p);
        expect_type(TYPE_PAIR, b);
        if (length(b) != 2)
            runtime_error("malformed binding in let: %s", stringify(b));
        Value ident = car(b), expr = cadr(b);
        expect_type( TYPE_SYMBOL, ident);
        letenv = table_inherit(letenv);
        Value val = eval(letenv, expr);
        env_put(letenv, ident, val);
    }
    return eval_body(letenv, body);
}

//PTR
static Value syn_let_star(Table *env, Value args)
{
    expect_arity_min(2, args);
    return let_star(env, car(args), cdr(args));
}

//PTR
static Value syn_letrec(Table *env, Value args)
{
    expect_arity_min(2, args);
    Value bindings = car(args);
    Value body = cdr(args);
    expect_list_head(bindings);
    expect_type(TYPE_PAIR, body);

    Table *letenv = table_inherit(env);
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        Value b = car(p);
        expect_type(TYPE_PAIR, b);
        Value ident = car(b);
        expect_type(TYPE_SYMBOL, ident);
        Value val = eval(letenv, cadr(b));
        env_put(letenv, ident, val);
    }
    return eval_body_tmpenv(letenv, body);
}

// 4.2.3. Sequencing
//PTR
static Value syn_begin(Table *env, Value body)
{
    return eval_body(env, body);
}

// 4.2.4. Iteration
//PTR
static Value syn_do(Table *env, Value args)
{
    expect_arity_min(2, args);

    Value bindings = car(args), tests = cadr(args), body = cddr(args);
    expect_list_head(bindings);
    expect_list_head(tests);
    Table *doenv = table_inherit(env);
    Value steps = Qnil;
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        Value b = car(p);
        expect_type(TYPE_PAIR, b);
        Value var = car(b), init = cadr(b), step = cddr(b);
        expect_type(TYPE_SYMBOL, var);
        if (step != Qnil)
            steps = cons(cons(var, car(step)), steps);
        env_put(doenv, var, eval(env, init)); // in the original env
    }
    Value test = car(tests), exprs = cdr(tests);
    while (eval(doenv, test) == Qfalse) {
        if (body != Qnil)
            eval_body(doenv, body);
        for (Value p = steps; p != Qnil; p = cdr(p)) {
            Value pstep = car(p);
            Value var = car(pstep), step = cdr(pstep);
            Value val = eval(doenv, step);
            iset(doenv, var, val);
        }
    }
    if (exprs != Qnil)
        return eval_body_tmpenv(doenv, exprs);
    table_free(doenv);
    return Qnil;
}

// 4.2.6. Quasiquotation
static Value qq_list(Table *env, Value datum, int64_t depth);

static Value qq(Table *env, Value datum, int64_t depth)
{
    if (depth == 0)
        return eval(env, datum);
    if (datum == Qnil || !value_is_pair(datum))
        return datum;
    Value a = car(datum), d = cdr(datum);
    if (a == SYM_QUASIQUOTE) {
        expect_type(TYPE_PAIR, d);
        Value v = qq(env, car(d), depth + 1);
        return list2(a, v);
    }
    if (a == SYM_UNQUOTE || a == SYM_UNQUOTE_SPLICING) {
        expect_type(TYPE_PAIR, d);
        Value v = qq(env, car(d), depth - 1);
        return depth == 1 ? v : list2(a, v);
    }
    return qq_list(env, datum, depth);
}

static Value last_pair(Value l)
{
    Value last = Qnil;
    for (Value p = l; p != Qnil; p = cdr(p))
        last = p;
    return last;
}

static bool is_quoted_terminal(Value list)
{
    Value a = car(list), d = cdr(list);
    return (a == SYM_UNQUOTE || a == SYM_QUASIQUOTE) &&
        d != Qnil && cdr(d) == Qnil;
}

static Value splicer(Value last, Value to_splice)
{
    expect_type(TYPE_PAIR, to_splice);
    if (last == Qnil)
        return to_splice;
    PAIR(last)->cdr = to_splice;
    return last_pair(to_splice);
}

static Value qq_list(Table *env, Value datum, int64_t depth)
{
    Value ret = DUMMY_PAIR();
    for (Value last = ret, p = datum; p != Qnil; p = cdr(p)) {
        bool is_simple = !value_is_pair(p);
        if (is_simple || is_quoted_terminal(p)) {
            expect_type(TYPE_PAIR, cdr(ret));
            PAIR(last)->cdr = is_simple ? p : qq(env, p, depth);
            break;
        }
        Value elem = car(p);
        bool spliced = (value_is_pair(elem) && car(elem) == SYM_UNQUOTE_SPLICING);
        Value v = qq(env, elem, depth);
        if (!spliced)
            last = PAIR(last)->cdr = list1(v);
        else if (v != Qnil)
            last = splicer(last, v);
    }
    return cdr(ret);
}

static Value syn_quasiquote(Table *env, Value datum)
{
    return qq(env, datum, 1);
}

static Value syn_unquote(UNUSED Table *env, UNUSED Value args)
{
    runtime_error("applied out of quasiquote (`)");
}

static Value syn_unquote_splicing(UNUSED Table *env, UNUSED Value args)
{
    runtime_error("applied out of quasiquote (`)");
}

// 5.2. Definitions
static Value define_variable(Table *env, Value ident, Value expr)
{
    expect_type(TYPE_SYMBOL, ident);

    Value val = eval(env, expr);
    bool found = false;
    if (env == toplevel_environment)
        found = env_set(env, ident, val);
    if (!found)
        env_put(env, ident, val); // prepend new
    return Qnil;
}

static Value define_proc_internal(Table *env, Value heads, Value body)
{
    Value ident = car(heads), params = cdr(heads);
    Value val = value_of_closure(env, params, body);
    return define_variable(env, ident, val);
}

static Value syn_define(Table *env, Value args)
{
    if (args == Qnil)
        runtime_error("wrong number of arguments: expected 1+");
    Value head = car(args);
    Type t = value_type_of(head);
    switch (t) {
    case TYPE_SYMBOL:
        expect_arity(2, args);
        return define_variable(env, head, cadr(args));
    case TYPE_PAIR:
        return define_proc_internal(env, head, cdr(args));
    case TYPE_NULL:
    case TYPE_BOOL:
    case TYPE_INT:
    case TYPE_STRING:
    case TYPE_PROC:
    case TYPE_UNDEF:
        runtime_error("the first argument expected symbol or pair but got %s",
                      value_type_to_string(t));
    }
    UNREACHABLE();
}

// 6.1. Equivalence predicates
static Value proc_eq(UNUSED Table *env, Value x, Value y)
{
    return OF_BOOL(x == y);
}

static bool equal(Value x, Value y)
{
    if (x == y)
        return true;
    Type tx = value_type_of(x), ty = value_type_of(y);
    if (tx != ty)
        return false;
    switch (tx) {
    case TYPE_PAIR:
        return equal(car(x), car(y)) &&
               equal(cdr(x), cdr(y));
    case TYPE_STRING:
        return (strcmp(STRING(x)->body, STRING(y)->body) == 0);
    case TYPE_SYMBOL:
    case TYPE_NULL:
    case TYPE_BOOL:
    case TYPE_INT:
    case TYPE_PROC:
    case TYPE_UNDEF:
        return false;
    }
    UNREACHABLE();
}

static Value proc_equal(UNUSED Table *env, Value x, Value y)
{
    return OF_BOOL(equal(x, y));
}

// 6.2.5. Numerical operations
static int64_t get_int(Value v)
{
    expect_type(TYPE_INT, v);
    return value_to_int(v);
}

static Value proc_integer_p(UNUSED Table *env, Value obj)
{
    return OF_BOOL(value_is_int(obj));
}

static Value proc_numeq(UNUSED Table *env, Value args)
{
    expect_arity_min(2, args);

    int64_t x = get_int(car(args));
    for (Value p = args; (p = cdr(p)) != Qnil; ) {
        if (get_int(car(p)) != x)
            return Qfalse;
    }
    return Qtrue;
}

static Value proc_lt(UNUSED Table *env, Value args)
{
    expect_arity_min(2, args);

    int64_t x = get_int(car(args)), y;
    for (Value p = args; (p = cdr(p)) != Qnil; x = y) {
        y = get_int(car(p));
        if (x >= y)
            return Qfalse;
    }
    return Qtrue;
}

static Value proc_gt(UNUSED Table *env, Value args)
{
    expect_arity_min(2, args);

    int64_t x = get_int(car(args)), y;
    for (Value p = args; (p = cdr(p)) != Qnil; x = y) {
        y = get_int(car(p));
        if (x <= y)
            return Qfalse;
    }
    return Qtrue;
}

static Value proc_le(UNUSED Table *env, Value args)
{
    expect_arity_min(2, args);

    int64_t x = get_int(car(args)), y;
    for (Value p = args; (p = cdr(p)) != Qnil; x = y) {
        y = get_int(car(p));
        if (x > y)
            return Qfalse;
    }
    return Qtrue;
}

static Value proc_ge(UNUSED Table *env, Value args)
{
    expect_arity_min(2, args);

    int64_t x = get_int(car(args)), y;
    for (Value p = args; (p = cdr(p)) != Qnil; x = y) {
        y = get_int(car(p));
        if (x < y)
            return Qfalse;
    }
    return Qtrue;
}

static Value proc_zero_p(UNUSED Table *env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && value_to_int(obj) == 0);
}

static Value proc_positive_p(UNUSED Table *env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && value_to_int(obj) > 0);
}

static Value proc_negative_p(UNUSED Table *env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && value_to_int(obj) < 0);
}

static Value proc_odd_p(UNUSED Table *env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && (value_to_int(obj) % 2) != 0);
}

static Value proc_even_p(UNUSED Table *env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && (value_to_int(obj) % 2) == 0);
}

static Value proc_max(UNUSED Table *env, Value args)
{
    expect_arity_min(1, args);
    int64_t max = get_int(car(args));
    for (Value p = cdr(args); p != Qnil; p = cdr(p)) {
        int64_t x = get_int(car(p));
        if (max < x)
            max = x;
    }
    return value_of_int(max);
}

static Value proc_min(UNUSED Table *env, Value args)
{
    expect_arity_min(1, args);
    int64_t min = get_int(car(args));
    for (Value p = cdr(args); p != Qnil; p = cdr(p)) {
        int64_t x = get_int(car(p));
        if (min > x)
            min = x;
    }
    return value_of_int(min);
}

static Value proc_add(UNUSED Table *env, Value args)
{
    int64_t y = 0;
    for (Value p = args; p != Qnil; p = cdr(p))
        y += get_int(car(p));
    return value_of_int(y);
}

static Value proc_sub(UNUSED Table *env, Value args)
{
    expect_arity_min(1, args);

    int64_t y = get_int(car(args));
    Value p = cdr(args);
    if (p == Qnil)
        return value_of_int(-y);
    for (; p != Qnil; p = cdr(p))
        y -= get_int(car(p));
    return value_of_int(y);
}

static Value proc_mul(UNUSED Table *env, Value args)
{
    int64_t y = 1;
    for (Value p = args; p != Qnil; p = cdr(p))
        y *= get_int(car(p));
    return value_of_int(y);
}

static Value proc_div(UNUSED Table *env, Value args)
{
    expect_arity_min(1, args);

    int64_t y = get_int(car(args));
    Value p = cdr(args);
    if (p == Qnil)
       return value_of_int(1 / y);
    for (; p != Qnil; p = cdr(p)) {
        int64_t x = get_int(car(p));
        if (x == 0)
            runtime_error("divided by zero");
        y /= x;
    }
    return value_of_int(y);
}

static Value proc_abs(UNUSED Table *env, Value x)
{
    int64_t n = get_int(x);
    return value_of_int(n < 0 ? -n : n);
}

static Value proc_quotient(UNUSED Table *env, Value x, Value y)
{
    int64_t b = get_int(y);
    if (b == 0)
        runtime_error("divided by zero");
    int64_t a = get_int(x);
    int64_t c = a / b;
    return value_of_int(c);
}


static Value proc_remainder(UNUSED Table *env, Value x, Value y)
{
    int64_t b = get_int(y);
    if (b == 0)
        runtime_error("divided by zero");
    int64_t a = get_int(x);
    int64_t c = a % b;
    return value_of_int(c);
}

static Value proc_modulo(UNUSED Table *env, Value x, Value y)
{
    int64_t b = get_int(y);
    if (b == 0)
        runtime_error("divided by zero");
    int64_t a = get_int(x);
    int64_t c = a % b;
    if ((a < 0 && b > 0) || (a > 0 && b < 0))
        c += b;
    return value_of_int(c);
}

static int64_t expt(int64_t x, int64_t y)
{
    int64_t z = 1;
    while (y > 0) {
        if ((y % 2) == 0) {
            x *= x;
            y /= 2;
        } else {
            z *= x;
            y--;
        }
    }
    return z;
}

static int64_t vexpt(Value x, Value y)
{
    int64_t b = get_int(y);
    if (b < 0)
        runtime_error("expt", "cannot power %d which negative", b);
    if (b == 0)
        return 1;
    int64_t a = get_int(x);
    if (a == 0)
        return 0;
    return expt(a, b);
}

static Value proc_expt(UNUSED Table *env, Value x, Value y)
{
    return value_of_int(vexpt(x, y));
}

// 6.3.1. Booleans
static Value proc_not(UNUSED Table *env, Value x)
{
    return OF_BOOL(x == Qfalse);
}

static Value proc_boolean_p(UNUSED Table *env, Value x)
{
    return OF_BOOL(x == Qtrue || x == Qfalse);
}

// 6.3.2. Pairs and lists
static Value proc_pair_p(UNUSED Table *env, Value o)
{
    return OF_BOOL(value_is_pair(o));
}

Value cons(Value car, Value cdr)
{
    Pair *p = obj_new(sizeof(Pair), TAG_PAIR);
    p->car = car;
    p->cdr = cdr;
    return (Value) p;
}

inline Value car(Value v)
{
    return PAIR(v)->car;
}

inline Value cdr(Value v)
{
    return PAIR(v)->cdr;
}

static Value proc_cons(UNUSED Table *env, Value car, Value cdr)
{
    return cons(car, cdr);
}

static Value proc_car(UNUSED Table *env, Value pair)
{
    expect_type(TYPE_PAIR, pair);
    return car(pair);
}

static Value proc_cdr(UNUSED Table *env, Value pair)
{
    expect_type(TYPE_PAIR, pair);
    return cdr(pair);
}

bool value_is_null(Value v)
{
    return v == Qnil;
}

static Value proc_null_p(UNUSED Table *env, Value list)
{
    return OF_BOOL(list == Qnil);
}

static Value proc_list_p(UNUSED Table *env, Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        if (!value_is_pair(p))
            return Qfalse;
    }
    return Qtrue;
}

static Value proc_list(UNUSED Table *env, Value args)
{
    return args;
}

int64_t length(Value l)
{
    int64_t len = 0;
    for (Value p = l; p != Qnil; p = cdr(p))
        len++;
    return len;
}

static Value proc_length(UNUSED Table *env, Value list)
{
    expect_list_head(list);
    return value_of_int(length(list));
}

static Value dup_list(Value l, Value *plast)
{
    if (l == Qnil) {
        *plast = Qnil;
        return Qnil;
    }
    Value dup = DUMMY_PAIR(), last = dup;
    for (Value p = l; p != Qnil; p = cdr(p)) {
        expect_type(TYPE_PAIR, p);
        last = PAIR(last)->cdr = list1(car(p));
    }
    *plast = last;
    return cdr(dup);
}

static Value proc_append(UNUSED Table *env, Value ls)
{
    Value l = Qnil, last = Qnil, p = ls;
    for (Value next; p != Qnil && (next = cdr(p)) != Qnil; p = next) {
        Value dup = dup_list(car(p), &last);
        l = append2(l, dup);
    }
    if (p != Qnil) {
        if (l == Qnil)
            l = car(p);
        else
            PAIR(last)->cdr = car(p);
    }
    return l;
}

Value reverse(Value l)
{
    Value ret = Qnil;
    for (Value p = l; p != Qnil; p = cdr(p))
        ret = cons(car(p), ret);
    return ret;
}

static Value proc_reverse(UNUSED Table *env, Value list)
{
    expect_list_head(list);
    return reverse(list);
}

static Value list_tail(Value list, Value k)
{
    expect_list_head(list);
    int64_t n = get_int(k);
    if (n < 0)
        runtime_error("%s: 2nd element needs to be non-negative: "PRId64, n);
    Value p = list;
    int64_t i;
    for (i = 0; p != Qnil; p = cdr(p), i++) {
        if (i == n)
            break;
    }
    if (i != n)
        runtime_error("list is shorter than %"PRId64"", n);
    return p;
}

static Value proc_list_tail(UNUSED Table *env, Value list, Value k)
{
    return list_tail(list, k);
}

static Value proc_list_ref(UNUSED Table *env, Value list, Value k)
{
    Value tail = list_tail(list, k);
    if (tail == Qnil)
        runtime_error("list is not longer than %"PRId64, value_to_int(k));
    return car(tail);
}

static Value memq(Value key, Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        Value e = car(p);
        if (e == key)
            return p;
    }
    return Qfalse;
}

static Value proc_memq(UNUSED Table *env, Value obj, Value list)
{
    expect_list_head(list);
    return memq(obj, list);
}

static Value member(Value key, Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        Value e = car(p);
        if (equal(e, key))
            return p;
    }
    return Qfalse;
}

static Value proc_member(UNUSED Table *env, Value obj, Value list)
{
    expect_list_head(list);
    return member(obj, list);
}

static Value assq(Value key, Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        Value e = car(p);
        if (value_is_pair(e) && car(e) == key)
            return e;
    }
    return Qfalse;
}

static Value proc_assq(UNUSED Table *env, Value obj, Value alist)
{
    expect_list_head(alist);
    return assq(obj, alist);
}

static Value assoc(Value key, Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        Value e = car(p);
        if (value_is_pair(e) && equal(car(e), key))
            return e;
    }
    return Qfalse;
}

static Value proc_assoc(UNUSED Table *env, Value obj, Value alist)
{
    expect_list_head(alist);
    return assoc(obj, alist);
}

// 6.3.3. Symbols
static Value proc_symbol_p(UNUSED Table *env, Value obj)
{
    return OF_BOOL(value_is_symbol(obj));
}

// 6.3.5. Strings
static Value proc_string_p(UNUSED Table *env, Value obj)
{
    return OF_BOOL(value_is_string(obj));
}

static Value proc_string_length(UNUSED Table *env, Value s)
{
    expect_type(TYPE_STRING, s);
    return value_of_int(strlen(STRING(s)->body));
}

static Value proc_string_eq(UNUSED Table *env, Value s1, Value s2)
{
    expect_type_twin(TYPE_STRING, s1, s2);
    return OF_BOOL(strcmp(STRING(s1)->body, STRING(s2)->body) == 0);
}

// 6.4. Control features
static Value proc_procedure_p(UNUSED Table *env, Value o)
{
    return OF_BOOL(value_is_procedure(o));
}

static Value build_apply_args(Value args)
{
    Value heads = DUMMY_PAIR(), p = args;
    for (Value last = heads, next; (next = cdr(p)) != Qnil; p = next)
        last = PAIR(last)->cdr = list1(car(p));
    Value rest = car(p);
    expect_list_head(rest);
    return append2(cdr(heads), rest);
}

static Value proc_apply(Table *env, Value args)
{
    expect_arity_min(2, args);

    Value proc = car(args);
    expect_type(TYPE_PROC, proc);
    Value appargs = build_apply_args(cdr(args));
    return apply(env, proc, appargs);
}

static bool cars_cdrs(Value ls, Value *pcars, Value *pcdrs)
{
    Value cars = DUMMY_PAIR(), cdrs = DUMMY_PAIR();
    for (Value p = ls, lcars = cars, lcdrs = cdrs; p != Qnil; p = cdr(p)) {
        Value l = car(p);
        if (l == Qnil)
            return false;
        expect_type(TYPE_PAIR, l);
        lcars = PAIR(lcars)->cdr = list1(car(l));
        lcdrs = PAIR(lcdrs)->cdr = list1(cdr(l));
    }
    *pcars = cdr(cars);
    *pcdrs = cdr(cdrs);
    return true;
}

static Value proc_map(Table *env, Value args)
{
    expect_arity_min(2, args);

    Value proc = car(args);
    expect_type(TYPE_PROC, proc);
    Value ls = cdr(args);
    Value ret = DUMMY_PAIR();
    for (Value last = ret, cars, cdrs; cars_cdrs(ls, &cars, &cdrs); ls = cdrs) {
        Value v = apply(env, proc, cars);
        last = PAIR(last)->cdr = list1(v);
    }
    return cdr(ret);
}

static Value proc_for_each(Table *env, Value args)
{
    expect_arity_min(2, args);

    Value proc = car(args);
    expect_type(TYPE_PROC, proc);
    Value lists = cdr(args);
    Value cars, cdrs;
    while (cars_cdrs(lists, &cars, &cdrs)) {
        apply(env, proc, cars);
        lists = cdrs;
    }
    return Qnil;
}

static Value value_of_continuation(void)
{
    Continuation *c = obj_new(sizeof(Continuation), TAG_CONTINUATION);
    c->proc.arity = 1; // by spec
    c->sp = c->stack = NULL;
    c->stack_len = 0;
    c->call_stack = Qnil;
    c->retval = Qfalse;
    return (Value) c;
}

ATTR(noinline)
static bool continuation_set(Value c)
{
    GET_SP(sp); // must be the first!
    Continuation *cont = CONTINUATION(c);
    cont->sp = sp;
    cont->stack_len = gc_stack_get_size(sp);
    cont->stack = xmalloc(cont->stack_len);
    UNPOISON(sp, cont->stack_len);
    memcpy(cont->stack, sp, cont->stack_len);
    cont->call_stack = call_stack;
    return setjmp(cont->state) != 0;
}

static Value proc_callcc(Table *env, Value proc)
{
    expect_type(TYPE_PROC, proc);
    Value c = value_of_continuation();
    if (continuation_set(c))
        return CONTINUATION(c)->retval;
    return apply(env, proc, list1(c));
}

// 6.6.3. Output
static void display_list(FILE *f, Value l)
{
    fprintf(f, "(");
    for (Value p = l, next; p != Qnil; p = next) {
        fdisplay(f, car(p));
        if ((next = cdr(p)) == Qnil)
            break;
        fprintf(f, " ");
        if (!value_is_pair(next)) {
            fprintf(f, ". ");
            fdisplay(f, next);
            break;
        }
    }
    fprintf(f, ")");
}

static void fdisplay(FILE* f, Value v)
{
    switch (value_type_of(v)) {
    case TYPE_NULL:
        fprintf(f, "()");
        break;
    case TYPE_BOOL:
        fprintf(f, "%s", v == Qtrue ? "#t" : "#f");
        break;
    case TYPE_INT:
        fprintf(f, "%"PRId64, value_to_int(v));
        break;
    case TYPE_SYMBOL:
    case TYPE_STRING:
        fprintf(f, "%s", value_to_string(v));
        break;
    case TYPE_PAIR:
        display_list(f, v);
        break;
    case TYPE_PROC:
        fprintf(f, "<procedure>");
        break;
    case TYPE_UNDEF:
        fprintf(f, "<undef>");
        break;
    }
}

char *stringify(Value v)
{
    char *s;
    size_t size;
    FILE *stream = open_memstream(&s, &size);
    if (stream == NULL)
        error("open_memstream() failed");
    fdisplay(stream, v);
    fclose(stream);
    return s;
}

void display(Value v)
{
    fdisplay(stdout, v);
}

static Value proc_display(UNUSED Table *env, Value obj)
{
    display(obj);
    return obj;
}

static Value proc_newline(void)
{
    puts("");
    return Qnil;
}

// 6.6.4. System interface
static Value proc_load(UNUSED Table *env, Value path)
{
    // Current spec: path is always relative
    return load_inner(value_to_string(path));
}

// Extensions from R7RS (scheme process-context)
ATTR(noreturn)
static Value proc_exit(UNUSED Table *env, Value args)
{
    expect_arity_range(0, 1, args);
    exit_status = 0;
    if (args != Qnil) {
        Value obj = car(args);
        if (obj == Qtrue)
            ; // use 0 for code as is
        else if (value_is_int(obj))
            exit_status = value_to_int(obj);
        else // or #f too
            exit_status = 2; // something failed
    }
    longjmp(jmp_exit, 1);
}

// Local Extensions
static Value proc_print(UNUSED Table *env, Value l)
{
    Value obj = Qnil;
    for (Value p = l, next; p != Qnil; p = next)  {
        obj = car(p);
        display(obj);
        next = cdr(p);
        if (next != Qnil)
            printf(" ");
    }
    puts("");
    return obj;
}

static Value proc_cputime(void) // in micro sec
{
    static const int64_t MICRO = 1000*1000;
    struct timespec t;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t);
    int64_t n = t.tv_sec * MICRO + lround(t.tv_nsec / 1000.0);
    return value_of_int(n);
}

static Value syn_defined_p(Table *env, Value name)
{
    if (!value_is_symbol(name))
        return Qfalse;
    return OF_BOOL(env_get(env, name) != Qundef);
}

int sch_fin(void)
{
    gc_fin();
    table_free(toplevel_environment);
    return exit_status;
}

int sch_exit_status(void)
{
    return exit_status;
}

#define DEF_CXR_BUILTIN(x, y) \
    static Value proc_c##x##y##r(UNUSED Table *env, Value v) \
    { \
        expect_type(TYPE_PAIR, v); \
        return c##x##y##r(v); \
    }
CXRS(DEF_CXR_BUILTIN)

void sch_init(uintptr_t *sp)
{
    gc_init(sp);

    static char basedir[PATH_MAX];
    load_basedir = getcwd(basedir, sizeof(basedir));
#define DEF_SYMBOL(var, name) SYM_##var = value_of_symbol(name)
    DEF_SYMBOL(ELSE, "else");
    DEF_SYMBOL(QUOTE, "quote");
    DEF_SYMBOL(QUASIQUOTE, "quasiquote");
    DEF_SYMBOL(UNQUOTE, "unquote");
    DEF_SYMBOL(UNQUOTE_SPLICING, "unquote-splicing");
    DEF_SYMBOL(RARROW, "=>");

    toplevel_environment = table_new();
    Table *e = toplevel_environment;

    // 4. Expressions

    // 4.1. Primitive expression types
    // 4.1.2. Literal expressions
    define_syntax(e, "quote", syn_quote, 1);
    // 4.1.4. Procedures
    define_syntax(e, "lambda", syn_lambda, -1);
    // 4.1.5. Conditionals
    define_syntax(e, "if", syn_if, -1);
    // 4.1.6. Assignments
    define_syntax(e, "set!", syn_set, 2);
    // 4.2. Derived expression types
    // 4.2.1. Conditionals
    define_syntax(e, "cond", syn_cond, -1);
    define_syntax(e, "case", syn_case, -1);
    define_syntax(e, "and", syn_and, -1);
    define_syntax(e, "or", syn_or, -1);
    // 4.2.2. Binding constructs
    define_syntax(e, "let", syn_let, -1); // with named let in 4.2.4.
    define_syntax(e, "let*", syn_let_star, -1);
    define_syntax(e, "letrec", syn_letrec, -1);
    // 4.2.3. Sequencing
    define_syntax(e, "begin", syn_begin, -1);
    // 4.2.4. Iteration
    define_syntax(e, "do", syn_do, -1);
    // 4.2.6. Quasiquotation
    define_syntax(e, "quasiquote", syn_quasiquote, 1);
    define_syntax(e, "unquote", syn_unquote, 1);
    define_syntax(e, "unquote-splicing", syn_unquote_splicing, 1);
    // 4.3. Macros
    // 4.3.2. Pattern language
    //- syntax-rules

    // 5. Program structure

    // 5.2. Definitions
    define_syntax(e, "define", syn_define, -1);
    // 5.3. Syntax definitions
    //- define-syntax

    // 6. Standard procedures

    // 6.1. Equivalence predicates
    define_procedure(e, "eqv?", proc_eq, 2); // alias
    define_procedure(e, "eq?", proc_eq, 2);
    define_procedure(e, "equal?", proc_equal, 2);
    // 6.2. Numbers
    // 6.2.5. Numerical operations
    define_procedure(e, "number?", proc_integer_p, 1); // alias
    define_procedure(e, "integer?", proc_integer_p, 1);
    define_procedure(e, "=", proc_numeq, -1);
    define_procedure(e, "<", proc_lt, -1);
    define_procedure(e, ">", proc_gt, -1);
    define_procedure(e, "<=", proc_le, -1);
    define_procedure(e, ">=", proc_ge, -1);
    define_procedure(e, "zero?", proc_zero_p, 1);
    define_procedure(e, "positive?", proc_positive_p, 1);
    define_procedure(e, "negative?", proc_negative_p, 1);
    define_procedure(e, "odd?", proc_odd_p, 1);
    define_procedure(e, "even?", proc_even_p, 1);
    define_procedure(e, "max", proc_max, -1);
    define_procedure(e, "min", proc_min, -1);
    define_procedure(e, "+", proc_add, -1);
    define_procedure(e, "*", proc_mul, -1);
    define_procedure(e, "-", proc_sub, -1);
    define_procedure(e, "/", proc_div, -1);
    define_procedure(e, "abs", proc_abs, 1);
    define_procedure(e, "quotient", proc_quotient, 2);
    define_procedure(e, "remainder", proc_remainder, 2);
    define_procedure(e, "modulo", proc_modulo, 2);
    define_procedure(e, "expt", proc_expt, 2);
    // 6.3. Other data types
    // 6.3.1. Booleans
    define_procedure(e, "not", proc_not, 1);
    define_procedure(e, "boolean?", proc_boolean_p, 1);
    // 6.3.2. Pairs and lists
    define_procedure(e, "pair?", proc_pair_p, 1);
    define_procedure(e, "cons", proc_cons, 2);
    define_procedure(e, "car", proc_car, 1);
    define_procedure(e, "cdr", proc_cdr, 1);
#define DEFUN_CXR(x, y) define_procedure(e, "c" #x #y "r", proc_c##x##y##r, 1)
    CXRS(DEFUN_CXR); // defines 28 procedures
    //- set-car!
    //- set-cdr!
    define_procedure(e, "null?", proc_null_p, 1);
    define_procedure(e, "list?", proc_list_p, 1);
    define_procedure(e, "list", proc_list, -1);
    define_procedure(e, "length", proc_length, 1);
    define_procedure(e, "append", proc_append, -1);
    define_procedure(e, "reverse", proc_reverse, 1);
    define_procedure(e, "list-tail", proc_list_tail, 2);
    define_procedure(e, "list-ref", proc_list_ref, 2);
    define_procedure(e, "memq", proc_memq, 2);
    define_procedure(e, "memv", proc_memq, 2); // alias
    define_procedure(e, "member", proc_member, 2);
    define_procedure(e, "assq", proc_assq, 2);
    define_procedure(e, "assv", proc_assq, 2); // alias
    define_procedure(e, "assoc", proc_assoc, 2); // alias
    // 6.3.3. Symbols
    define_procedure(e, "symbol?", proc_symbol_p, 1);
    // 6.3.5. Strings
    define_procedure(e, "string?", proc_string_p, 1);
    define_procedure(e, "string-length", proc_string_length, 1);
    define_procedure(e, "string=?", proc_string_eq, 2);
    // 6.4. Control features
    define_procedure(e, "procedure?", proc_procedure_p, 1);
    define_procedure(e, "apply", proc_apply, -1);
    define_procedure(e, "map", proc_map, -1);
    define_procedure(e, "for-each", proc_for_each, -1);
    define_procedure(e, "call/cc", proc_callcc, 1); // alias
    define_procedure(e, "call-with-current-continuation", proc_callcc, 1);
    //- values
    //- call-with-values
    //- dynamic-wind
    // 6.5. Eval
    //- eval
    //- scheme-report-environment
    //- null-environment
    // 6.6. Input and output
    // 6.6.2. Input
    //- read
    // 6.6.3. Output
    define_procedure(e, "display", proc_display, 1);
    define_procedure(e, "newline", proc_newline, 0);
    // 6.6.4. System interface
    define_procedure(e, "load", proc_load, 1);

    // Extensions from R7RS (scheme process-context)
    define_procedure(e, "exit", proc_exit, -1);

    // Local Extensions
    define_procedure(e, "print", proc_print, -1); // like Gauche
    define_procedure(e, "_cputime", proc_cputime, 0);
    define_syntax(e, "_defined?", syn_defined_p, 1);
}
