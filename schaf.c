#include <errno.h>
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
    [TYPE_ENV] = "environment",
    [TYPE_PORT] = "port",
};

#define OF_BOOL(v) ((!!(v) << 3U) | 0b100U)

// Value (uintptr_t):
//   0b.....000 Pointer (Unchangeable pattern!)
//   0b.......1 Integer
//   0b......10 Symbol
//   0b0--00100 #f
//   0b0--01100 #t
//   0b0-010100 <undef>
typedef uintptr_t Symbol;
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
static Value env_toplevel, env_default, env_r5rs, env_null;
static Value symbol_names = Qnil; // ("name0" "name1" ...)
Value SYM_ELSE, SYM_QUOTE, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING,
    SYM_RARROW;
static const char *load_basedir = NULL;
static Value source_data = Qnil; // (a)list of AST: (filename syntax_list newline_positions)
                                 // newline_positions: list of pos | int
static jmp_buf jmp_exit;
static uint8_t exit_status; // should be <= 125 to be portable
static char errmsg[BUFSIZ];

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
    case TAG_VECTOR:
    case TAG_ENV:
    case TAG_PORT:
        return false;
    case TAG_ERROR:
        break;
    }
    UNREACHABLE();
}

inline bool value_is_pair(Value v)
{
    return value_tag_is(v, TAG_PAIR);
}

inline static bool is_error(Value v)
{
    return value_tag_is(v, TAG_ERROR);
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
    case TAG_VECTOR:
        return TYPE_VECTOR;
    case TAG_ENV:
        return TYPE_ENV;
    case TAG_PORT:
        return TYPE_PORT;
    case TAG_ERROR:
        break;
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

inline static Symbol value_to_symbol(Value v)
{
    return (Symbol) v >> FLAG_NBIT_SYM;
}

static const char *name_nth(Value list, int64_t n)
{
    Value p = list;
    for (int64_t i = 0; i < n; i++) {
        if ((p = cdr(p)) == Qnil)
            return NULL;
    }
    return STRING(car(p))->body;
}

static const char *unintern(Symbol sym)
{
    const char *name = name_nth(symbol_names, (int64_t) sym);
    if (name == NULL) // fatal; every known symbols should have a name
        bug("symbol %lu not found", sym);
    return name;
}

inline const char *value_to_string(Value v)
{
    if (value_is_symbol(v))
        return unintern(value_to_symbol(v));
    return STRING(v)->body;
}

// define caar, cadr, ... cddddr, 28 procedures, at once
#define CXR1(f, x) f(a, x) f(d, x)
#define CXR2(f, x) CXR1(f, a ## x) CXR1(f, d ## x)
#define CXR3(f, x) CXR2(f, a ## x) CXR2(f, d ## x)
#define CXR4(f, x) CXR3(f, a) CXR3(f, d)
#define CXRS(f) CXR2(f,) CXR3(f,) CXR4(f,)

#define DEF_CXR(x, y) \
    static Value c##x##y##r(Value v) { return c##x##r(c##y##r(v)); }
CXRS(DEF_CXR)

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
    Header *h = HEADER(p);
    h->tag = t;
    h->immutable = false;
    return p;
}

static Value value_of_string_move(char *s)
{
    String *str = obj_new(sizeof(String), TAG_STRING);
    str->body = s; // move ownership and use as is
    return (Value) str;
}

Value value_of_string(const char *s)
{
    return value_of_string_move(xstrdup(s));
}

static void expect_cfunc_arity(int64_t actual)
{
    if (actual <= CFUNCARG_MAX)
        return;
    bug("arity too large: expected ..%"PRId64" but got %"PRId64,
        CFUNCARG_MAX, actual);
}

static Value apply_cfunc_v(Value env, Value f, Value args)
{
    return CFUNC(f)->f1(env, args);
}

static Value apply_cfunc_0(Value env, Value f, UNUSED Value args)
{
    return CFUNC(f)->f0(env);
}

static Value apply_cfunc_1(Value env, Value f, Value args)
{
    return CFUNC(f)->f1(env, car(args));
}

static Value apply_cfunc_2(Value env, Value f, Value args)
{
    return CFUNC(f)->f2(env, car(args), cadr(args));
}

static Value apply_cfunc_3(Value env, Value f, Value args)
{
    return CFUNC(f)->f3(env, car(args), cadr(args), caddr(args));
}

static Value cfunc_new(ValueTag tag, const char *name, void *cfunc, int64_t arity)
{
    expect_cfunc_arity(arity);
    CFunc *f = obj_new(sizeof(CFunc), tag);
    f->proc.arity = arity;
    f->name = xstrdup(name);
    f->cfunc = cfunc;
    switch (arity) {
    case -1:
        f->proc.apply = apply_cfunc_v;
        break;
    case 0:
        f->proc.apply = apply_cfunc_0;
        break;
    case 1:
        f->proc.apply = apply_cfunc_1;
        break;
    case 2:
        f->proc.apply = apply_cfunc_2;
        break;
    case 3:
        f->proc.apply = apply_cfunc_3;
        break;
    default:
        bug("invalid arity: %"PRId64, arity);
    }
    return (Value) f;
}

static Value value_of_cfunc(const char *name, void *cfunc, int64_t arity)
{
    return cfunc_new(TAG_CFUNC, name, cfunc, arity);
}

static Value value_of_syntax(const char *name, void *cfunc, int64_t arity)
{
    return cfunc_new(TAG_SYNTAX, name, cfunc, arity);
}

static Value eval_body(Value env, Value body);
static Value env_inherit(Value parent);
static Value env_put(Value env, Value key, Value value);

//PTR
static Value apply_closure(UNUSED Value env, Value proc, Value args)
{
    Closure *cl = CLOSURE(proc);
    int64_t arity = cl->proc.arity;
    Value clenv = env_inherit(cl->env);
    Value params = cl->params;
    if (arity == -1)
        env_put(clenv, params, args);
    else {
        for (Value pa = args, pp = params; pa != Qnil; pa = cdr(pa), pp = cdr(pp))
            env_put(clenv, car(pp), car(pa));
    }
    return eval_body(clenv, cl->body);
}

static Value value_of_closure(Value env, Value params, Value body)
{
    Closure *f = obj_new(sizeof(Closure), TAG_CLOSURE);
    f->proc.arity = (params == Qnil || value_is_pair(params)) ? length(params) : -1;
    f->proc.apply = apply_closure;
    f->env = env;
    f->params = params;
    f->body = body;
    return (Value) f;
}

// and `cons` is well-known name than "value_of_pair"

//
// Errors
//

[[gnu::noreturn]]
void raise_error(jmp_buf buf, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(errmsg, sizeof(errmsg), fmt, ap);
    va_end(ap);
    longjmp(buf, 1);
}

// value_of_error() or
static Value runtime_error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(errmsg, sizeof(errmsg), fmt, ap);
    va_end(ap);

    Error *e = obj_new(sizeof(Error), TAG_ERROR);
    e->call_stack = Qnil;
    return (Value) e;
}

const char *error_message(void)
{
    return errmsg;
}

// generic macro to handle errors and early-returns
#define CHECK_ERROR(v) do { \
        Value V = v; \
        if (UNLIKELY(is_error(V))) \
            return V; \
    } while (0)
#define CHECK_ERROR_TRUTHY(v) do { \
        Value V = v; \
        if (UNLIKELY((V) != Qfalse)) \
            return V; \
    } while (0)
#define CHECK_ERROR_LOCATED(v, l) do { \
        Value V = v; \
        if (UNLIKELY(is_error(V))) { \
            if (ERROR(V)->call_stack == Qnil) \
                ERROR(V)->call_stack = list1(cons(Qfalse, l)); \
            return V; \
        } \
    } while (0)
#define EXPECT(f, ...) CHECK_ERROR(expect_##f(__VA_ARGS__))

static Value expect_type(Type expected, Value v)
{
    Type t = value_type_of(v);
    if (LIKELY(t == expected))
        return Qfalse;
    return runtime_error("type expected %s but got %s",
                         value_type_to_string(expected), value_type_to_string(t));
}

static Value expect_type_twin(Type expected, Value x, Value y)
{
    Value v = expect_type(expected, x);
    if (UNLIKELY(is_error(v)))
        return v;
    return expect_type(expected, y);
}

static Value expect_type_or(Type e1, Type e2, Value v)
{
    Type t = value_type_of(v);
    if (LIKELY(t == e1 || t == e2))
        return Qfalse;
    return runtime_error("type expected %s or %s but got %s",
                         value_type_to_string(e1), value_type_to_string(e2),
                         value_type_to_string(t));
}

static Value expect_arity_range(int64_t min, int64_t max, Value args)
{
    int64_t actual = length(args);
    if (LIKELY(min <= actual && actual <= max))
        return Qfalse;
    return runtime_error("wrong number of arguments: expected %"PRId64"..%"PRId64
                         " but got %"PRId64, min, max, actual);
}

static Value expect_arity_min(int64_t min, Value args)
{
    int64_t actual = length(args);
    if (LIKELY(min <= actual))
        return Qfalse;
    return runtime_error("wrong number of arguments: expected >= %"PRId64" but got %"PRId64,
                         min, actual);
}

static Value expect_arity(int64_t expected, Value args)
{
    int64_t actual = length(args);
    if (LIKELY(expected < 0 || expected == actual))
        return Qfalse;
    return runtime_error("wrong number of arguments: expected %"PRId64" but got %"PRId64,
                         expected, actual);
}

static Value env_new(const char *name)
{
    Env *e = obj_new(sizeof(Env), TAG_ENV);
    e->name = name == NULL ? NULL : xstrdup(name);
    e->table = table_new();
    e->parent = Qfalse;
    return (Value) e;
}

static Value env_dup(const char *name, const Value orig)
{
    if (ENV(orig)->parent != Qfalse)
        bug("duplication of chained environment not permitted");
    Env *e = obj_new(sizeof(Env), TAG_ENV);
    if (name == NULL)
        name = ENV(orig)->name; // copy it
    e->name = xstrdup(name);
    e->table = table_dup(ENV(orig)->table);
    e->parent = Qfalse;
    return (Value) e;
}

static Value env_inherit(Value parent)
{
    Value e = env_new("tmp");
    ENV(e)->parent = parent;
    return e;
}

static Value env_put(Value env, Value key, Value value)
{
    table_put(ENV(env)->table, value_to_symbol(key), value);
    return env;
}

// chained!
static bool env_set(Value env, Value key, Value value)
{
    Symbol sym = value_to_symbol(key);
    for (Value p = env; p != Qfalse; p = ENV(p)->parent) {
        if (table_set(ENV(p)->table, sym, value))
            return true;
    }
    return false;
}

// chained!!
static Value env_get(const Value env, Value name)
{
    Symbol sym = value_to_symbol(name);
    for (Value p = env; p != Qfalse; p = ENV(p)->parent) {
        Value v = table_get(ENV(p)->table, sym);
        if (v != TABLE_NOT_FOUND)
            return v;
    }
    return Qundef;
}

// Note: Do not mistake this for "(define-syntax ...)" which related to macros
static void define_syntax(Value env, const char *name, void *cfunc, int64_t arity)
{
    env_put(env, value_of_symbol(name), value_of_syntax(name, cfunc, arity));
}

static void define_procedure(Value env, const char *name, void *cfunc, int64_t arity)
{
    env_put(env, value_of_symbol(name), value_of_cfunc(name, cfunc, arity));
}

//
// Evaluation
//
static Value eval(Value env, Value v);

static Value eval_body(Value env, Value body)
{
    Value last = Qnil;
    for (Value p = body; p != Qnil; p = cdr(p)) {
        last = eval(env, car(p));
        CHECK_ERROR(last);
    }
    return last;
}

static Value map_eval(Value env, Value l)
{
    Value mapped = DUMMY_PAIR();
    for (Value last = mapped, p = l, v; p != Qnil; p = cdr(p)) {
        v = eval(env, car(p));
        CHECK_ERROR(v);
        last = PAIR(last)->cdr = list1(v);
    }
    return cdr(mapped);
}

static Value push_stack_frame(Value ve, const char *name, Value loc)
{
    Error *e = ERROR(ve);
    Value str = name == NULL ? Qfalse : value_of_string(name);
    Value data = cons(str, loc);
    e->call_stack = cons(data, e->call_stack);
    return ve;
}

static Value apply(Value env, Value proc, Value args)
{
    EXPECT(arity, PROCEDURE(proc)->arity, args);
    return PROCEDURE(proc)->apply(env, proc, args);
}

static Value eval_apply(Value env, Value l)
{
    Value symproc = car(l), args = cdr(l);
    Value proc = eval(env, symproc);
    CHECK_ERROR_LOCATED(proc, l);
    EXPECT(type, TYPE_PROC, proc);
    if (!value_tag_is(proc, TAG_SYNTAX)) {
        args = map_eval(env, args);
        CHECK_ERROR_LOCATED(args, l);
    }
    Value ret = apply(env, proc, args);
    if (UNLIKELY(is_error(ret))) {
        const char *fname = (VALUE_TAG(proc) == TAG_CFUNC) ?
            CFUNC(proc)->name : NULL;
        return push_stack_frame(ret, fname, l);
    }
    return ret;
}

static Value lookup_or_error(Value env, Value v)
{
    Value p = env_get(env, v);
    if (p == Qundef)
        return runtime_error("unbound variable: %s", value_to_string(v));
    return p;
}

static Value eval(Value env, Value v)
{
    if (value_is_symbol(v))
        return lookup_or_error(env, v);
    if (v == Qnil || !value_is_pair(v))
        return v;
    return eval_apply(env, v);
}

[[gnu::format(printf, 1, 2)]]
static int append_error_message(const char *fmt, ...)
{
    size_t len = strlen(errmsg);
    va_list ap;
    va_start(ap, fmt);
    int n = vsnprintf(errmsg + len, sizeof(errmsg) - len, fmt, ap);
    va_end(ap);
    return n;
}

static Value assoc(Value key, Value l);

static void dump_line_column(Value filename, int64_t pos)
{
    int64_t line, col;
    Value data = assoc(filename, source_data);
    if (data == Qnil) {
        append_error_message("\n\t<unknown>");
        return;
    }
    Value newline_pos = caddr(data);
    pos_to_line_col(pos, newline_pos, &line, &col);
    append_error_message("\n\t%s:%"PRId64":%"PRId64" in ",
                         STRING(filename)->body, line, col);
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
    Value sym = cadar(callers);
    if (!value_is_symbol(sym)) {
        append_error_message("<unknown>");
        return;
    }
    const char *name = value_to_string(sym);
    append_error_message("'%s'", name);
}

static void dump_frame(Value data, Value callers)
{
    Value pair = cdr(data);
    Value filename = find_filename(pair);
    if (filename == Qfalse) {
        append_error_message("\n\t<unknown>");
        return;
    }
    dump_line_column(filename, LOCATED_PAIR(pair)->pos);
    dump_callee_name(callers);
}

static void prepend_cfunc_name(Value v)
{
    if (v == Qfalse)
        return;
    const char *s = STRING(v)->body;
    size_t len = strlen(s) + 2;// strlen(": ")
    char tmp[sizeof(errmsg) - len];
    snprintf(tmp, sizeof(tmp), "%s", errmsg);
    snprintf(errmsg, sizeof(errmsg), "%s: %s", s, tmp);
}

static void dump_stack_trace(Value call_stack)
{
    if (call_stack == Qnil)
        return;
    Value ordered = reverse(call_stack);
    prepend_cfunc_name(caar(ordered));
    for (Value p = ordered, next; p != Qnil; p = next) {
        next = cdr(p);
        dump_frame(car(p), next);
    }
}

static void fdisplay(FILE* f, Value v);

static Value iload(FILE *in, const char *filename)
{
    Value ast = iparse(in, filename);
    if (ast == Qundef)
        return Qundef;
    source_data = cons(ast, source_data);
    if (setjmp(jmp_exit) != 0)
        return value_of_int(exit_status);
    Value ret = eval_body(env_toplevel, cadr(ast));
    if (is_error(ret)) {
        dump_stack_trace(ERROR(ret)->call_stack);
        return Qundef;
    }
    return ret;
}

static Value iload_inner(FILE *in, const char *path)
{
    Value ast = iparse(in, path);
    if (ast == Qundef)
        return Qundef;
    source_data = cons(ast, source_data);
    return eval_body(env_toplevel, cadr(ast));
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
    const char *p = path;
    if (path[0] != '/') {
        snprintf(joined, sizeof(joined), "%s/%s", load_basedir, path);
        p = joined;
    }
    if (realpath(p, rpath) == NULL)
        return NULL;

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
        error("load: can't open file: %s", path);
    Value retval = iload(in, path);
    fclose(in);
    return retval;
}

static Value load_inner(const char *path)
{
    const char *basedir_saved = load_basedir;
    FILE *in = open_loadable(path);
    if (in == NULL)
        return runtime_error("load: can't open file: %s", path);
    Value retval = iload_inner(in, path);
    fclose(in);
    load_basedir = basedir_saved;
    return retval;
}

//
// Built-in Procedures / Syntax
//

// 4. Expressions
// 4.1. Primitive expression types
// 4.1.2. Literal expressions
static Value syn_quote(UNUSED Value env, Value datum)
{
    return datum;
}

// 4.1.4. Procedures
//PTR -- proper tail recursion needed
static Value syn_lambda(Value env, Value args)
{
    Value params = car(args), body = cdr(args);
    if (params != Qnil)
        EXPECT(type_or, TYPE_PAIR, TYPE_SYMBOL, params);
    EXPECT(type, TYPE_PAIR, body);
    return value_of_closure(env, params, body);
}

// 4.1.5. Conditionals
//PTR
static Value syn_if(Value env, Value args)
{
    EXPECT(arity_range, 2, 3, args);

    Value cond = car(args), then = cadr(args);
    Value v = eval(env, cond);
    CHECK_ERROR(v);
    if (v != Qfalse)
        return eval(env, then);
    Value els = cddr(args);
    if (els == Qnil)
        return Qfalse;
    return eval(env, car(els));
}

// 4.1.6. Assignments
static Value iset(Value env, Value ident, Value val)
{
    bool found = env_set(env, ident, val);
    if (!found)
        return runtime_error("unbound variable: %s", value_to_string(ident));
    return Qfalse;
}

static Value syn_set(Value env, Value ident, Value expr)
{
    EXPECT(type, TYPE_SYMBOL, ident);
    Value v = eval(env, expr);
    CHECK_ERROR(v);
    return iset(env, ident, v);
}

// 4.2. Derived expression types
// 4.2.1. Conditionals
static Value cond_eval_recipient(Value env, Value test, Value recipients)
{
    EXPECT(type, TYPE_PAIR, recipients);
    Value recipient = eval(env, car(recipients)), rest = cdr(recipients);
    CHECK_ERROR(recipient);
    EXPECT(type, TYPE_PROC, recipient);
    if (rest != Qnil)
        return runtime_error("only one expression expected after =>");
    return apply(env, recipient, list1(test));
}

//PTR
static Value syn_cond(Value env, Value clauses)
{
    EXPECT(arity_min, 1, clauses);

    for (Value p = clauses; p != Qnil; p = cdr(p)) {
        Value clause = car(p);
        EXPECT(type, TYPE_PAIR, clause);
        Value test = car(clause);
        Value exprs = cdr(clause);
        if (test == SYM_ELSE)
            return eval_body(env, exprs);
        Value t = eval(env, test);
        CHECK_ERROR(t);
        if (t != Qfalse) {
            if (exprs == Qnil)
                return t;
            if (car(exprs) == SYM_RARROW)
                return cond_eval_recipient(env, t, cdr(exprs));
            return eval_body(env, exprs);
        }
    }
    return Qfalse;
}

static Value expect_list_head(Value v)
{
    EXPECT(type_or, TYPE_NULL, TYPE_PAIR, v);
    return Qfalse;
}

static Value memq(Value key, Value l);

//PTR
static Value syn_case(Value env, Value args)
{
    EXPECT(arity_min, 2, args);
    Value key = eval(env, car(args)), clauses = cdr(args);
    CHECK_ERROR(key);
    EXPECT(list_head, clauses);

    for (Value p = clauses; p != Qnil; p = cdr(p)) {
        Value clause = car(p);
        EXPECT(type, TYPE_PAIR, clause);
        Value data = car(clause), exprs = cdr(clause);
        EXPECT(type, TYPE_PAIR, exprs);
        if (data == SYM_ELSE)
            return eval_body(env, exprs);
        EXPECT(type, TYPE_PAIR, data);
        if (memq(key, data) != Qfalse)
            return eval_body(env, exprs);
    }
    return Qfalse;
}

//PTR
static Value syn_and(Value env, Value args)
{
    Value last = Qtrue;
    for (Value p = args; p != Qnil; p = cdr(p)) {
        if ((last = eval(env, car(p))) == Qfalse)
            break;
        CHECK_ERROR(last);
    }
    return last;
}

//PTR
static Value syn_or(Value env, Value args)
{
    Value last = Qfalse;
    for (Value p = args; p != Qnil; p = cdr(p)) {
        if ((last = eval(env, car(p))) != Qfalse) // include errors
            break;
    }
    return last;
}

// 4.2.2. Binding constructs
static Value transpose_2xn(Value ls, Value *pfirsts, Value *pseconds) // 2 * n
{
    Value firsts = DUMMY_PAIR(), seconds = DUMMY_PAIR();
    for (Value lfirsts = firsts, lseconds = seconds; ls != Qnil; ls = cdr(ls)) {
        Value l = car(ls);
        EXPECT(arity, 2, l);
        lfirsts = PAIR(lfirsts)->cdr = list1(car(l));
        lseconds = PAIR(lseconds)->cdr = list1(cadr(l));
    }
    *pfirsts = cdr(firsts);
    *pseconds = cdr(seconds);
    return Qfalse;
}

static Value let(Value env, Value var, Value bindings, Value body)
{
    EXPECT(list_head, bindings);
    Value params = Qnil, symargs = Qnil;
    Value r = transpose_2xn(bindings, &params, &symargs);
    CHECK_ERROR(r);
    Value args = map_eval(env, symargs);
    CHECK_ERROR(args);
    if (var == Qfalse) {
        Value proc = value_of_closure(env, params, body);
        return apply_closure(Qfalse, proc, args);
    }
    Value letenv = env_inherit(env);
    Value proc = value_of_closure(letenv, params, body);
    env_put(letenv, var, proc); // affects as proc->env
    return apply_closure(Qfalse, proc, args);
}

//PTR
static Value syn_let(Value env, Value args)
{
    EXPECT(arity_min, 2, args);
    Value bind_or_var = car(args), body = cdr(args);
    Value var = Qfalse, bindings = bind_or_var;
    if (value_is_symbol(bind_or_var)) {
        var = bind_or_var;
        bindings = car(body);
        body = cdr(body);
    }
    return let(env, var, bindings, body);
}

static Value let_star(Value env, Value bindings, Value body)
{
    EXPECT(list_head, bindings);
    Value letenv = env;
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        Value b = car(p);
        EXPECT(type, TYPE_PAIR, b);
        if (length(b) != 2)
            return runtime_error("malformed binding in let: %s", stringify(b));
        Value ident = car(b), expr = cadr(b);
        EXPECT(type, TYPE_SYMBOL, ident);
        letenv = env_inherit(letenv);
        Value val = eval(letenv, expr);
        CHECK_ERROR(val);
        env_put(letenv, ident, val);
    }
    return eval_body(letenv, body);
}

//PTR
static Value syn_let_star(Value env, Value args)
{
    EXPECT(arity_min, 2, args);
    return let_star(env, car(args), cdr(args));
}

//PTR
static Value syn_letrec(Value env, Value args)
{
    EXPECT(arity_min, 2, args);
    Value bindings = car(args);
    Value body = cdr(args);
    EXPECT(list_head, bindings);
    EXPECT(type, TYPE_PAIR, body);

    Value letenv = env_inherit(env);
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        Value b = car(p);
        EXPECT(type, TYPE_PAIR, b);
        Value ident = car(b);
        EXPECT(type, TYPE_SYMBOL, ident);
        Value val = eval(letenv, cadr(b));
        CHECK_ERROR(val);
        env_put(letenv, ident, val);
    }
    return eval_body(letenv, body);
}

// 4.2.3. Sequencing
//PTR
static Value syn_begin(Value env, Value body)
{
    return eval_body(env, body);
}

// 4.2.4. Iteration
//PTR
static Value syn_do(Value env, Value args)
{
    EXPECT(arity_min, 2, args);

    Value bindings = car(args), tests = cadr(args), body = cddr(args);
    EXPECT(list_head, bindings);
    EXPECT(list_head, tests);
    Value doenv = env_inherit(env);
    Value steps = Qnil, v;
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        Value b = car(p);
        EXPECT(type, TYPE_PAIR, b);
        Value var = car(b), init = cadr(b), step = cddr(b);
        EXPECT(type, TYPE_SYMBOL, var);
        if (step != Qnil)
            steps = cons(cons(var, car(step)), steps);
        v = eval(env, init);
        CHECK_ERROR(v);
        env_put(doenv, var, v); // in the original env
    }
    Value test = car(tests), exprs = cdr(tests);
    while ((v = eval(doenv, test)) == Qfalse) {
        if (body != Qnil) {
            v = eval_body(doenv, body);
            CHECK_ERROR(v);
        }
        for (Value p = steps; p != Qnil; p = cdr(p)) {
            Value pstep = car(p);
            Value var = car(pstep), step = cdr(pstep);
            Value val = eval(doenv, step);
            CHECK_ERROR(val);
            iset(doenv, var, val);
        }
    }
    CHECK_ERROR(v);
    return eval_body(doenv, exprs);
}

// 4.2.6. Quasiquotation
static Value qq_list(Value env, Value datum, int64_t depth);

static Value qq(Value env, Value datum, int64_t depth)
{
    if (depth == 0)
        return eval(env, datum);
    if (datum == Qnil || !value_is_pair(datum))
        return datum;
    Value a = car(datum), d = cdr(datum);
    if (a == SYM_QUASIQUOTE) {
        EXPECT(type, TYPE_PAIR, d);
        Value v = qq(env, car(d), depth + 1);
        CHECK_ERROR(v);
        return list2_const(a, v);
    }
    if (a == SYM_UNQUOTE || a == SYM_UNQUOTE_SPLICING) {
        EXPECT(type, TYPE_PAIR, d);
        Value v = qq(env, car(d), depth - 1);
        CHECK_ERROR(v);
        return depth == 1 ? v : list2_const(a, v);
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
    EXPECT(type, TYPE_PAIR, to_splice);
    if (last == Qnil)
        return to_splice;
    PAIR(last)->cdr = to_splice;
    return last_pair(to_splice);
}

static Value qq_list(Value env, Value datum, int64_t depth)
{
    Value ret = DUMMY_PAIR();
    for (Value last = ret, p = datum; p != Qnil; p = cdr(p)) {
        bool is_simple = !value_is_pair(p);
        if (is_simple || is_quoted_terminal(p)) {
            EXPECT(type, TYPE_PAIR, cdr(ret));
            if (!is_simple) {
                p = qq(env, p, depth);
                CHECK_ERROR(p);
            }
            PAIR(last)->cdr = p;
            break;
        }
        Value elem = car(p);
        bool spliced = (value_is_pair(elem) && car(elem) == SYM_UNQUOTE_SPLICING);
        Value v = qq(env, elem, depth);
        CHECK_ERROR(v);
        if (!spliced)
            last = PAIR(last)->cdr = list1_const(v);
        else if (v != Qnil)
            last = splicer(last, v);
    }
    return cdr(ret);
}

static Value syn_quasiquote(Value env, Value datum)
{
    return qq(env, datum, 1);
}

static Value syn_unquote(UNUSED Value env, UNUSED Value args)
{
    return runtime_error("applied out of quasiquote (`)");
}

static Value syn_unquote_splicing(UNUSED Value env, UNUSED Value args)
{
    return runtime_error("applied out of quasiquote (`)");
}

// 4.3. Macros
// 4.3.2. Pattern language
//- syntax-rules

// 5. Program structure
// 5.2. Definitions
static Value define_variable(Value env, Value ident, Value expr)
{
    EXPECT(type, TYPE_SYMBOL, ident);

    Value val = eval(env, expr);
    CHECK_ERROR(val);
    bool found = false;
    if (env == env_toplevel)
        found = env_set(env, ident, val);
    if (!found)
        env_put(env, ident, val); // prepend new
    return Qfalse;
}

static Value define_proc_internal(Value env, Value heads, Value body)
{
    Value ident = car(heads), params = cdr(heads);
    Value val = value_of_closure(env, params, body);
    return define_variable(env, ident, val);
}

static Value syn_define(Value env, Value args)
{
    if (args == Qnil)
        return runtime_error("wrong number of arguments: expected >= 1 but got 0");
    Value head = car(args);
    Type t = value_type_of(head);
    switch (t) {
    case TYPE_SYMBOL:
        EXPECT(arity, 2, args);
        return define_variable(env, head, cadr(args));
    case TYPE_PAIR:
        return define_proc_internal(env, head, cdr(args));
    case TYPE_NULL:
    case TYPE_BOOL:
    case TYPE_INT:
    case TYPE_STRING:
    case TYPE_PROC:
    case TYPE_VECTOR:
    case TYPE_ENV:
    case TYPE_PORT:
    case TYPE_UNDEF:
        return runtime_error("the first argument expected symbol or pair but got %s",
                             value_type_to_string(t));
    }
    UNREACHABLE();
}

// 5.3. Syntax definitions
//- define-syntax

// 6. Standard procedures
// 6.1. Equivalence predicates
static Value proc_eq(UNUSED Value env, Value x, Value y)
{
    return OF_BOOL(x == y);
}

static bool equal(Value x, Value y);

static bool vector_equal(const Vector *x, const Vector *y)
{
    if (x->length != y->length)
        return false;
    for (int64_t i = 0; i < x->length; i++) {
        if (!equal(x->body[i], y->body[i]))
            return false;
    }
    return true;
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
    case TYPE_VECTOR:
        return vector_equal(VECTOR(x), VECTOR(y));
    case TYPE_SYMBOL:
    case TYPE_NULL:
    case TYPE_BOOL:
    case TYPE_INT:
    case TYPE_PROC:
    case TYPE_ENV:
    case TYPE_PORT:
    case TYPE_UNDEF:
        return false;
    }
    UNREACHABLE();
}

static Value proc_equal(UNUSED Value env, Value x, Value y)
{
    return OF_BOOL(equal(x, y));
}

// 6.2. Numbers
// 6.2.5. Numerical operations
static Value proc_integer_p(UNUSED Value env, Value obj)
{
    return OF_BOOL(value_is_int(obj));
}

#define get_int(x) ({ \
            Value X = x; \
            EXPECT(type, TYPE_INT, X); \
            value_to_int(X); \
        })

typedef bool (*RelOpFunc)(int64_t x, int64_t y);
static Value relop(RelOpFunc func, Value args)
{
    EXPECT(arity_min, 2, args);

    int64_t x = get_int(car(args));
    while ((args = cdr(args)) != Qnil) {
        int64_t y = get_int(car(args));
        if (!func(x, y))
            return Qfalse;
        x = y;
    }
    return Qtrue;
}

static inline bool relop_eq(int64_t x, int64_t y) { return x == y; }
static inline bool relop_lt(int64_t x, int64_t y) { return x <  y; }
static inline bool relop_le(int64_t x, int64_t y) { return x <= y; }
static inline bool relop_gt(int64_t x, int64_t y) { return x >  y; }
static inline bool relop_ge(int64_t x, int64_t y) { return x >= y; }

static Value proc_numeq(UNUSED Value env, Value args)
{
    return relop(relop_eq, args);
}

static Value proc_lt(UNUSED Value env, Value args)
{
    return relop(relop_lt, args);
}

static Value proc_gt(UNUSED Value env, Value args)
{
    return relop(relop_gt, args);
}

static Value proc_le(UNUSED Value env, Value args)
{
    return relop(relop_le, args);
}

static Value proc_ge(UNUSED Value env, Value args)
{
    return relop(relop_ge, args);
}

static Value proc_zero_p(UNUSED Value env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && value_to_int(obj) == 0);
}

static Value proc_positive_p(UNUSED Value env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && value_to_int(obj) > 0);
}

static Value proc_negative_p(UNUSED Value env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && value_to_int(obj) < 0);
}

static Value proc_odd_p(UNUSED Value env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && (value_to_int(obj) % 2) != 0);
}

static Value proc_even_p(UNUSED Value env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && (value_to_int(obj) % 2) == 0);
}

static Value proc_max(UNUSED Value env, Value args)
{
    EXPECT(arity_min, 1, args);
    int64_t max = get_int(car(args));
    for (Value p = cdr(args); p != Qnil; p = cdr(p)) {
        int64_t x = get_int(car(p));
        if (max < x)
            max = x;
    }
    return value_of_int(max);
}

static Value proc_min(UNUSED Value env, Value args)
{
    EXPECT(arity_min, 1, args);
    int64_t min = get_int(car(args));
    for (Value p = cdr(args); p != Qnil; p = cdr(p)) {
        int64_t x = get_int(car(p));
        if (min > x)
            min = x;
    }
    return value_of_int(min);
}

static Value proc_add(UNUSED Value env, Value args)
{
    int64_t y = 0;
    for (Value p = args; p != Qnil; p = cdr(p))
        y += get_int(car(p));
    return value_of_int(y);
}

static Value proc_sub(UNUSED Value env, Value args)
{
    EXPECT(arity_min, 1, args);

    int64_t y = get_int(car(args));
    Value p = cdr(args);
    if (p == Qnil)
        return value_of_int(-y);
    for (; p != Qnil; p = cdr(p))
        y -= get_int(car(p));
    return value_of_int(y);
}

static Value proc_mul(UNUSED Value env, Value args)
{
    int64_t y = 1;
    for (Value p = args; p != Qnil; p = cdr(p))
        y *= get_int(car(p));
    return value_of_int(y);
}

static Value proc_div(UNUSED Value env, Value args)
{
    EXPECT(arity_min, 1, args);

    int64_t y = get_int(car(args));
    Value p = cdr(args);
    if (p == Qnil)
       return value_of_int(1 / y);
    for (; p != Qnil; p = cdr(p)) {
        int64_t x = get_int(car(p));
        if (x == 0)
            return runtime_error("divided by zero");
        y /= x;
    }
    return value_of_int(y);
}

static Value proc_abs(UNUSED Value env, Value x)
{
    int64_t n = get_int(x);
    return value_of_int(n < 0 ? -n : n);
}

static Value proc_quotient(UNUSED Value env, Value x, Value y)
{
    int64_t a = get_int(x), b = get_int(y);
    if (b == 0)
        return runtime_error("divided by zero");
    return value_of_int(a / b);
}

static Value proc_remainder(UNUSED Value env, Value x, Value y)
{
    int64_t a = get_int(x), b = get_int(y);
    if (b == 0)
        return runtime_error("divided by zero");
    int64_t c = a % b;
    return value_of_int(c);
}

static Value proc_modulo(UNUSED Value env, Value x, Value y)
{
    int64_t a = get_int(x), b = get_int(y);
    if (b == 0)
        return runtime_error("divided by zero");
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

static Value proc_expt(UNUSED Value env, Value x, Value y)
{
    int64_t a = get_int(x), b = get_int(y);
    if (b < 0)
        return runtime_error("cannot power %d which negative", b);
    int64_t c;
    if (b == 0)
        c = 1;
    else if (a == 0)
        c = 0;
    else
        c = expt(a, b);
    return value_of_int(c);
}

// 6.2.6. Numerical input and output
static Value proc_number_to_string(UNUSED Value env, Value x)
{
    char buf[22]; // log10(UINT64_MAX)+2
    EXPECT(type, TYPE_INT, x);
    int64_t n = value_to_int(x);
    snprintf(buf, sizeof(buf), "%"PRId64, n);
    return value_of_string(buf);
}

// 6.3. Other data types
// 6.3.1. Booleans
static Value proc_not(UNUSED Value env, Value x)
{
    return OF_BOOL(x == Qfalse);
}

static Value proc_boolean_p(UNUSED Value env, Value x)
{
    return OF_BOOL(x == Qtrue || x == Qfalse);
}

// 6.3.2. Pairs and lists
static Value proc_pair_p(UNUSED Value env, Value o)
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

static Value proc_cons(UNUSED Value env, Value car, Value cdr)
{
    return cons(car, cdr);
}

static Value proc_car(UNUSED Value env, Value pair)
{
    EXPECT(type, TYPE_PAIR, pair);
    return car(pair);
}

static Value proc_cdr(UNUSED Value env, Value pair)
{
    EXPECT(type, TYPE_PAIR, pair);
    return cdr(pair);
}

static Value proc_set_car(UNUSED Value env, Value pair, Value obj)
{
    EXPECT(type, TYPE_PAIR, pair);
    if (HEADER(pair)->immutable)
        return runtime_error("cannot modify immutable pair");
    PAIR(pair)->car = obj;
    return pair;
}

static Value proc_set_cdr(UNUSED Value env, Value pair, Value obj)
{
    EXPECT(type, TYPE_PAIR, pair);
    if (HEADER(pair)->immutable)
        return runtime_error("cannot modify immutable pair");
    PAIR(pair)->cdr = obj;
    return pair;
}

#define DEF_CXR_BUILTIN(x, y) \
    static Value proc_c##x##y##r(UNUSED Value env, Value v) \
    { \
        EXPECT(type, TYPE_PAIR, v); \
        return c##x##y##r(v); \
    }
CXRS(DEF_CXR_BUILTIN)

bool value_is_null(Value v)
{
    return v == Qnil;
}

static Value proc_null_p(UNUSED Value env, Value list)
{
    return OF_BOOL(list == Qnil);
}

static Value proc_list_p(UNUSED Value env, Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        if (!value_is_pair(p))
            return Qfalse;
    }
    return Qtrue;
}

static Value proc_list(UNUSED Value env, Value args)
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

static Value proc_length(UNUSED Value env, Value list)
{
    EXPECT(list_head, list);
    return value_of_int(length(list));
}

static Value dup_list(Value l, Value *plast)
{
    if (l == Qnil)
        return Qnil;
    Value dup = DUMMY_PAIR(), last = dup;
    for (Value p = l; p != Qnil; p = cdr(p)) {
        EXPECT(type, TYPE_PAIR, p);
        last = PAIR(last)->cdr = list1(car(p));
    }
    *plast = last;
    return cdr(dup);
}

static Value proc_append(UNUSED Value env, Value ls)
{
    Value l = DUMMY_PAIR(), last = l, p = ls;
    for (Value next, nlast = Qnil; p != Qnil && (next = cdr(p)) != Qnil; p = next) {
        Value pl = car(p);
        EXPECT(list_head, pl);
        Value dup = dup_list(pl, &nlast);
        if (dup != Qnil) {
            PAIR(last)->cdr = dup;
            last = nlast;
        }
    }
    l = cdr(l);
    if (p != Qnil) {
        if (l == Qnil)
            return car(p);
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

static Value proc_reverse(UNUSED Value env, Value list)
{
    EXPECT(list_head, list);
    return reverse(list);
}

static Value list_tail(Value list, Value k)
{
    EXPECT(list_head, list);
    int64_t n = get_int(k);
    if (n < 0)
        return runtime_error("2nd element needs to be non-negative: "PRId64, n);
    Value p = list;
    int64_t i;
    for (i = 0; p != Qnil; p = cdr(p), i++) {
        if (i == n)
            break;
    }
    if (i != n)
        return runtime_error("list is shorter than %"PRId64"", n);
    return p;
}

static Value proc_list_tail(UNUSED Value env, Value list, Value k)
{
    return list_tail(list, k);
}

static Value proc_list_ref(UNUSED Value env, Value list, Value k)
{
    Value tail = list_tail(list, k);
    if (tail == Qnil)
        return runtime_error("list is not longer than %"PRId64, value_to_int(k));
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

static Value proc_memq(UNUSED Value env, Value obj, Value list)
{
    EXPECT(list_head, list);
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

static Value proc_member(UNUSED Value env, Value obj, Value list)
{
    EXPECT(list_head, list);
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

static Value proc_assq(UNUSED Value env, Value obj, Value alist)
{
    EXPECT(list_head, alist);
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

static Value proc_assoc(UNUSED Value env, Value obj, Value alist)
{
    EXPECT(list_head, alist);
    return assoc(obj, alist);
}

// 6.3.3. Symbols
static Value proc_symbol_p(UNUSED Value env, Value obj)
{
    return OF_BOOL(value_is_symbol(obj));
}

// 6.3.5. Strings
static Value proc_string_p(UNUSED Value env, Value obj)
{
    return OF_BOOL(value_is_string(obj));
}

static Value proc_string_length(UNUSED Value env, Value s)
{
    EXPECT(type, TYPE_STRING, s);
    return value_of_int(strlen(STRING(s)->body));
}

static Value proc_string_eq(UNUSED Value env, Value s1, Value s2)
{
    EXPECT(type_twin, TYPE_STRING, s1, s2);
    return OF_BOOL(strcmp(STRING(s1)->body, STRING(s2)->body) == 0);
}

static Value proc_string_append(UNUSED Value env, Value args)
{
    EXPECT(list_head, args);
    size_t len = 0;
    for (Value p = args, v; p != Qnil; p = cdr(p)) {
        v = car(p);
        EXPECT(type, TYPE_STRING, v);
        len += strlen(STRING(v)->body);
    }
    char *s = xmalloc(len + 1);
    s[0] = '\0';
    for (Value p = args; p != Qnil; p = cdr(p))
        strcat(s, STRING(car(p))->body);
    return value_of_string_move(s);
}

// 6.3.6. Vectors
Value vector_new(void)
{
    const int64_t INIT_LEN = 2;
    Vector *v = obj_new(sizeof(Vector), TAG_VECTOR);
    v->length = 0;
    v->capacity = INIT_LEN;
    v->body = xmalloc(sizeof(Value) * v->capacity);
    return (Value) v;
}

Value vector_push(Value vv, Value e)
{
    Vector *v = VECTOR(vv);
    if (v->length == v->capacity) {
        v->capacity *= 2;
        v->body = xrealloc(v->body, sizeof(Value) * v->capacity);
    }
    v->body[v->length++] = e;
    return vv;
}

static Value proc_vector_p(UNUSED Value env, Value o)
{
    return OF_BOOL(value_tag_is(o, TAG_VECTOR));
}

static Value proc_vector(UNUSED Value env, Value args)
{
    Value v = vector_new();
    for (Value p = args; p != Qnil; p = cdr(p))
        vector_push(v, car(p));
    return v;
}

static Value proc_vector_ref(UNUSED Value env, Value o, Value k)
{
    EXPECT(type, TYPE_VECTOR, o);
    int64_t i = get_int(k);
    if (i < 0)
        return runtime_error("invalid index: negative integer %"PRId64, i);
    Vector *v = VECTOR(o);
    if (i >= v->length)
        return Qfalse;
    return v->body[i];
}

// 6.4. Control features
static Value proc_procedure_p(UNUSED Value env, Value o)
{
    return OF_BOOL(value_is_procedure(o));
}

static Value build_apply_args(Value args)
{
    Value heads = DUMMY_PAIR(), p = args, last = heads;
    for (Value next; (next = cdr(p)) != Qnil; p = next)
        last = PAIR(last)->cdr = list1(car(p));
    Value rest = car(p);
    EXPECT(list_head, rest);
    PAIR(last)->cdr = rest;
    return cdr(heads);
}

static Value proc_apply(Value env, Value args)
{
    EXPECT(arity_min, 2, args);

    Value proc = car(args);
    EXPECT(type, TYPE_PROC, proc);
    Value appargs = build_apply_args(cdr(args));
    CHECK_ERROR(appargs);
    return apply(env, proc, appargs);
}

static Value cars_cdrs(Value ls, Value *pcars, Value *pcdrs, Value *perr)
{
    Value cars = DUMMY_PAIR(), cdrs = DUMMY_PAIR();
    for (Value p = ls, lcars = cars, lcdrs = cdrs; p != Qnil; p = cdr(p)) {
        Value l = car(p);
        if (l == Qnil)
            return false;
        Value e = expect_type(TYPE_PAIR, l);
        if (UNLIKELY(is_error(e))) {
            *perr = e;
            return false;
        }
        lcars = PAIR(lcars)->cdr = list1(car(l));
        lcdrs = PAIR(lcdrs)->cdr = list1(cdr(l));
    }
    *pcars = cdr(cars);
    *pcdrs = cdr(cdrs);
    return true;
}

static Value proc_map(Value env, Value args)
{
    EXPECT(arity_min, 2, args);

    Value proc = car(args);
    EXPECT(type, TYPE_PROC, proc);
    Value ls = cdr(args);
    Value ret = DUMMY_PAIR(), e = Qfalse;
    for (Value last = ret, cars, cdrs, v; cars_cdrs(ls, &cars, &cdrs, &e); ls = cdrs) {
        v = apply(env, proc, cars);
        CHECK_ERROR(v);
        last = PAIR(last)->cdr = list1(v);
    }
    CHECK_ERROR_TRUTHY(e);
    return cdr(ret);
}

static Value proc_for_each(Value env, Value args)
{
    EXPECT(arity_min, 2, args);

    Value proc = car(args), e = Qfalse;
    EXPECT(type, TYPE_PROC, proc);
    for (Value ls = cdr(args), cars, cdrs, v; cars_cdrs(ls, &cars, &cdrs, &e); ls = cdrs) {
        v = apply(env, proc, cars);
        CHECK_ERROR(v);
    }
    CHECK_ERROR_TRUTHY(e);
    return Qfalse;
}

[[gnu::noreturn, gnu::noinline]]
static void jump(Continuation *cont)
{
    memcpy(cont->sp, cont->stack, cont->stack_len);
    longjmp(cont->state, 1);
}

#define GET_SP(p) uintptr_t v##p = 0, *p = &v##p; UNPOISON(&p, sizeof(uintptr_t *))

[[gnu::noreturn, gnu::noinline]]
static Value apply_continuation(UNUSED Value env, Value f, Value args)
{
    GET_SP(sp);
    Continuation *cont = CONTINUATION(f);
    cont->retval = PROCEDURE(f)->arity == 1 ? car(args) : args;
    int64_t d = sp - cont->sp;
    if (d < 1)
        d = 1;
    volatile uintptr_t pad[d];
    pad[0] = pad[d-1] = 0; // avoid unused
    jump(cont);
}

static Value value_of_continuation(int64_t n)
{
    Continuation *c = obj_new(sizeof(Continuation), TAG_CONTINUATION);
    c->proc.arity = n; // call/cc: 1, call-with-values: -1
    c->proc.apply = apply_continuation;
    c->sp = c->stack = NULL;
    c->stack_len = 0;
    c->retval = Qfalse;
    return (Value) c;
}

[[gnu::noinline]]
static bool continuation_set(Value c)
{
    GET_SP(sp); // must be the first!
    Continuation *cont = CONTINUATION(c);
    cont->sp = sp;
    cont->stack_len = gc_stack_get_size(sp);
    cont->stack = xmalloc(cont->stack_len);
    UNPOISON(sp, cont->stack_len);
    memcpy(cont->stack, sp, cont->stack_len);
    return setjmp(cont->state) != 0;
}

static Value proc_callcc(Value env, Value proc)
{
    EXPECT(type, TYPE_PROC, proc);
    Value c = value_of_continuation(1);
    if (continuation_set(c))
        return CONTINUATION(c)->retval;
    return apply(env, proc, list1(c));
}

static Value inner_continuation = Qfalse; // Yeah we live in the land of single-thread

static Value proc_values(Value env, Value args)
{
    if (inner_continuation == Qfalse)
        return car(args);
    return apply(env, inner_continuation, args);
}

static Value proc_call_with_values(Value env, Value producer, Value consumer)
{
    EXPECT(type_twin, TYPE_PROC, producer, consumer);

    Value k = value_of_continuation(-1), origk = inner_continuation;
    Value args;
    if (continuation_set(k))
        args = CONTINUATION(k)->retval;
    else {
        inner_continuation = k;
        Value val = apply(env, producer, Qnil);
        CHECK_ERROR(val);
        args = list1(val);
    }
    inner_continuation = origk;
    return apply(env, consumer, args);
}

// 6.5. Eval
static Value proc_eval(UNUSED Value genv, Value expr, Value env)
{
    EXPECT(type, TYPE_ENV, env);
    return eval(env, expr);
}

static Value proc_scheme_report_environment(UNUSED Value env, Value version)
{
    if (!value_is_int(version) || value_to_int(version) != 5)
        return runtime_error("only integer '5' is allowed for argument");
    return env_dup(NULL, env_r5rs);
}

static Value proc_null_environment(UNUSED Value env, Value version)
{
    if (!value_is_int(version) || value_to_int(version) != 5)
        return runtime_error("only integer '5' is allowed for argument");
    return env_dup(NULL, env_null);
}

static Value proc_interaction_environment(UNUSED Value env)
{
    return env_dup("interaction", env_default); // alias
}

// 6.6. Input and output
// 6.6.1. Ports
static Value proc_port_p(UNUSED Value env, Value port)
{
    return OF_BOOL(value_tag_is(port, TAG_PORT));
}

static Value value_of_port(FILE *fp)
{
    Port *p = obj_new(sizeof(Port), TAG_PORT);
    p->fp = fp;
    return (Value) p;
}

static Value port_stdin(void)
{
    static Value p = Qfalse;
    if (p == Qfalse)
        p = value_of_port(stdin);
    return p;
}

static Value port_stdout(void)
{
    static Value p = Qfalse;
    if (p == Qfalse)
        p = value_of_port(stdout);
    return p;
}

static Value proc_current_input_port(UNUSED Value env)
{
    return port_stdin();
}

static Value proc_current_output_port(UNUSED Value env)
{
    return port_stdout();
}

static Value proc_open_input_file(UNUSED Value env, Value vpath)
{
    EXPECT(type, TYPE_STRING, vpath);
    const char *path = STRING(vpath)->body;
    FILE *fp = fopen(path, "r");
    if (fp == NULL)
        return runtime_error("cannot open file: %s", path);
    return value_of_port(fp);
}

static void close_port(Port *p)
{
    if (p->fp == NULL)
        return;
    fclose(p->fp);
    p->fp = NULL; // guarantee safety
}

static Value proc_close_port(UNUSED Value env, Value port)
{
    EXPECT(type, TYPE_PORT, port);
    close_port(PORT(port));
    return Qfalse;
}

// 6.6.2. Input
static Value iread(FILE *in)
{
    Value eof = Qfalse; // temporary
    Value datum = parse_datum(in, "<read>");
    if (datum == Qundef)
        return eof;
    return datum;
}

static Value proc_read(UNUSED Value env, Value args)
{
    EXPECT(arity_range, 0, 1, args);
    Value port;
    if (args == Qnil)
        port = port_stdin();
    else {
        port = car(args);
        EXPECT(type, TYPE_PORT, port);
    }
    return iread(PORT(port)->fp);
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

static void display_vector(FILE *f, Value vv)
{
    Vector *v = VECTOR(vv);
    fprintf(f, "#(");
    for (int64_t i = 0; i < v->length; i++) {
        Value e = v->body[i];
        fdisplay(f, e);
        if (i + 1 == v->length)
            break;
        fprintf(f, " ");
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
    case TYPE_VECTOR:
        display_vector(f, v);
        break;
    case TYPE_ENV:
        fprintf(f, "<environment: %s>", ENV(v)->name);
        break;
    case TYPE_PORT:
        fprintf(f, "<port>");
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
    errno = 0;
    FILE *stream = open_memstream(&s, &size);
    if (stream == NULL) {
        perror(NULL);
        exit(2);
    }
    fdisplay(stream, v);
    fclose(stream);
    return s;
}

void display(Value v)
{
    fdisplay(stdout, v);
}

static Value proc_display(UNUSED Value env, Value obj)
{
    display(obj);
    return obj;
}

static Value proc_newline(UNUSED Value env)
{
    puts("");
    return Qfalse;
}

// 6.6.4. System interface
static Value proc_load(UNUSED Value env, Value path)
{
    EXPECT(type, TYPE_STRING, path);
    // Current spec: path is always relative
    return load_inner(STRING(path)->body);
}

// Extensions from R7RS (scheme process-context)
static Value proc_exit(UNUSED Value env, Value args)
{
    EXPECT(arity_range, 0, 1, args);
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
static Value syn_defined_p(Value env, Value name)
{
    if (!value_is_symbol(name))
        return Qfalse;
    return OF_BOOL(env_get(env, name) != Qundef);
}

static Value proc_print(UNUSED Value env, Value l)
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

static Value proc_cputime(UNUSED Value env) // in micro sec
{
    static const int64_t MICRO = 1000*1000;
    struct timespec t;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t);
    int64_t n = t.tv_sec * MICRO + lround(t.tv_nsec / 1000.0);
    return value_of_int(n);
}

static Value proc_schaf_environment(UNUSED Value env)
{
    return env_dup(NULL, env_default);
}

static void free_source_data(void)
{
    for (Value p = source_data; p != Qnil; p = cdr(p)) {
        Value l = car(p);
        free(STRING(car(l))->body);
    }
}

static void free_symbol_names(void)
{
    for (Value p = symbol_names; p != Qnil; p = cdr(p))
        free(STRING(car(p))->body);
}

static void env_free(Value ve)
{
    Env *e = ENV(ve);
    free(e->name);
    table_free(e->table);
}

int sch_fin(void)
{
    env_free(env_r5rs);
    env_free(env_default);
    env_free(env_toplevel);
    free_source_data();
    free_symbol_names();
    gc_fin();
    return exit_status;
}

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

    env_toplevel = env_new("default");
    Value e = env_toplevel;

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
    env_null = env_dup("null", e);

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
    // 6.2.6. Numerical input and output
    define_procedure(e, "number->string", proc_number_to_string, 1);
    // 6.3. Other data types
    // 6.3.1. Booleans
    define_procedure(e, "not", proc_not, 1);
    define_procedure(e, "boolean?", proc_boolean_p, 1);
    // 6.3.2. Pairs and lists
    define_procedure(e, "pair?", proc_pair_p, 1);
    define_procedure(e, "cons", proc_cons, 2);
    define_procedure(e, "car", proc_car, 1);
    define_procedure(e, "cdr", proc_cdr, 1);
#define DEFUN_CXR(x, y) define_procedure(e, "c" #x #y "r", proc_c##x##y##r, 1);
    CXRS(DEFUN_CXR); // registers 28 procedures
    define_procedure(e, "set-car!", proc_set_car, 2);
    define_procedure(e, "set-cdr!", proc_set_cdr, 2);
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
    define_procedure(e, "string-append", proc_string_append, -1);
    // 6.3.6. Vectors
    define_procedure(e, "vector?", proc_vector_p, 1);
    define_procedure(e, "vector", proc_vector, -1);
    define_procedure(e, "vector-ref", proc_vector_ref, 2);

    // 6.4. Control features
    define_procedure(e, "procedure?", proc_procedure_p, 1);
    define_procedure(e, "apply", proc_apply, -1);
    define_procedure(e, "map", proc_map, -1);
    define_procedure(e, "for-each", proc_for_each, -1);
    define_procedure(e, "call/cc", proc_callcc, 1); // alias
    define_procedure(e, "call-with-current-continuation", proc_callcc, 1);
    define_procedure(e, "values", proc_values, -1);
    define_procedure(e, "call-with-values", proc_call_with_values, 2);
    //- dynamic-wind
    // 6.5. Eval
    define_procedure(e, "eval", proc_eval, 2);
    define_procedure(e, "scheme-report-environment", proc_scheme_report_environment, 1);
    define_procedure(e, "null-environment", proc_null_environment, 1);
    define_procedure(e, "interaction-environment", proc_interaction_environment, 0);
    // 6.6. Input and output
    // 6.6.1. Ports
    define_procedure(e, "port?", proc_port_p, 1); // R5RS missing the definition!!
    define_procedure(e, "current-input-port", proc_current_input_port, 0);
    define_procedure(e, "current-output-port", proc_current_output_port, 0);
    define_procedure(e, "open-input-file", proc_open_input_file, 1);
    define_procedure(e, "close-port", proc_close_port, 1);
    define_procedure(e, "close-output-port", proc_close_port, 1); // alias
    define_procedure(e, "close-input-port", proc_close_port, 1);  // alias
    // 6.6.2. Input
    define_procedure(e, "read", proc_read, -1);
    // 6.6.3. Output
    define_procedure(e, "display", proc_display, 1);
    define_procedure(e, "newline", proc_newline, 0);
    // 6.6.4. System interface
    define_procedure(e, "load", proc_load, 1);

    env_r5rs = env_dup("r5rs", e);

    // Extensions from R7RS (scheme process-context)
    define_procedure(e, "exit", proc_exit, -1);

    // Local Extensions
    define_syntax(e, "_defined?", syn_defined_p, 1);
    define_procedure(e, "print", proc_print, -1); // like Gauche
    define_procedure(e, "_cputime", proc_cputime, 0);
    define_procedure(e, "schaf-environment", proc_schaf_environment, 0);

    env_default = env_dup("default", e);
}
