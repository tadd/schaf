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
Value SYM_QUOTE;
static const char *load_basedir = NULL;
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

static Value apply_cfunc_v(Table *env, CFunc *f, Value args)
{
    return f->f1(env, args);
}

static Value apply_cfunc_0(Table *env, CFunc *f, UNUSED Value args)
{
    return f->f0(env);
}

static Value apply_cfunc_1(Table *env, CFunc *f, Value args)
{
    Value a = car(args);
    return f->f1(env, a);
}

static Value apply_cfunc_2(Table *env, CFunc *f, Value args)
{
    Value p = args, a0, a1;
    a0 = car(p); p = cdr(p);
    a1 = car(p);
    return f->f2(env, a0, a1);
}

static Value apply_cfunc_3(Table *env, CFunc *f, Value args)
{
    Value p = args, a0, a1, a2;
    a0 = car(p); p = cdr(p);
    a1 = car(p); p = cdr(p);
    a2 = car(p);
    return f->f3(env, a0, a1, a2);
}

static Value value_of_cfunc(const char *name, void *cfunc, int64_t arity)
{
    expect_cfunc_arity(arity);
    CFunc *f = obj_new(sizeof(CFunc), TAG_CFUNC);
    f->name = xstrdup(name);
    f->proc.arity = arity;
    f->cfunc = cfunc;
    switch (arity) {
    case -1:
        f->applier = apply_cfunc_v;
        break;
    case 0:
        f->applier = apply_cfunc_0;
        break;
    case 1:
        f->applier = apply_cfunc_1;
        break;
    case 2:
        f->applier = apply_cfunc_2;
        break;
    case 3:
        f->applier = apply_cfunc_3;
        break;
    default:
        error("invalid arity: %"PRId64, arity);
    }
    return (Value) f;
}

static Value value_of_syntax(const char *name, void *cfunc, int64_t arity)
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

static jmp_buf jmp_exit;
static uint8_t exit_status; // to be portable
static char errmsg[BUFSIZ];

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
#define CHECK_ERROR(v)  do { \
        if (UNLIKELY(is_error(v))) \
            return v; \
    } while (0)
#define CHECK_ERROR_TRUTHY(v)  do { \
        if (UNLIKELY((v) != Qfalse)) \
            return v; \
    } while (0)
#define CHECK_ERROR_LOCATED(v, l)  do { \
        if (UNLIKELY(is_error(v))) { \
            if (ERROR(v)->call_stack == Qnil) \
                ERROR(v)->call_stack = list1(cons(Qfalse, l)); \
            return v; \
        } \
    } while (0)
#define EXPECT(f, ...) do { \
        Value R = expect_##f(__VA_ARGS__); \
        CHECK_ERROR(R); \
    } while (0)

static Value expect_type(Type expected, Value v)
{
    Type t = value_type_of(v);
    if (LIKELY(t == expected))
        return Qfalse;
    return runtime_error("type expected %s but got %s",
                         value_type_to_string(expected), value_type_to_string(t));
}
#define expect_type_twin(t, x, y) expect_type(t, x); expect_type(t, y)

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

static Value apply_cfunc(Table *env, Value proc, Value args)
{
    CFunc *f = CFUNC(proc);
    return f->applier(env, f, args);
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

[[gnu::noreturn]] [[gnu::noinline]]
static void jump(Continuation *cont)
{
    memcpy(cont->sp, cont->stack, cont->stack_len);
    longjmp(cont->state, 1);
}

#define GET_SP(p) uintptr_t v##p = 0, *p = &v##p; UNPOISON(&p, sizeof(uintptr_t *))

[[gnu::noreturn]] [[gnu::noinline]]
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
    EXPECT(arity, PROCEDURE(proc)->arity, args);
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
static void define_syntax(Table *env, const char *name, void *cfunc, int64_t arity)
{
    env_put(env, value_of_symbol(name), value_of_syntax(name, cfunc, arity));
}

static void define_procedure(Table *env, const char *name, void *cfunc, int64_t arity)
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
    Value c##x##y##r(Value v) { return c##x##r(c##y##r(v)); }
CXRS(DEF_CXR)

//
// Evaluation
//
static Value eval(Table *env, Value v);

static Value eval_body(Table *env, Value body)
{
    Value last = Qnil;
    for (Value p = body; p != Qnil; p = cdr(p)) {
        last = eval(env, car(p));
        CHECK_ERROR(last);
    }
    return last;
}

static Value map_eval(Table *env, Value l)
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

static Value eval_apply(Table *env, Value l)
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

static Value lookup_or_error(Table *env, Value v)
{
    Value p = env_get(env, v);
    if (p == Qundef)
        return runtime_error("unbound variable: %s", value_to_string(v));
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
    Value ast = iparse(in, filename), l = cadr(ast);
    if (l == Qundef)
        return Qundef;
    source_data = cons(ast, source_data);
    if (setjmp(jmp_exit) != 0)
        return value_of_int(exit_status);
    Value ret = eval_body(toplevel_environment, l);
    if (is_error(ret)) {
        dump_stack_trace(ERROR(ret)->call_stack);
        return Qundef;
    }
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
        return runtime_error("load: can't open file: %s", path);
    Value retval = iload_inner(in, path);
    fclose(in);
    load_basedir = basedir_saved;
    return retval;
}

//
// Built-in Procedures / Syntax
//

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
        EXPECT(type_or, TYPE_PAIR, TYPE_SYMBOL, params);
    EXPECT(type, TYPE_PAIR, body);
    return value_of_closure(env, params, body);
}

// 4.1.5. Conditionals
//PTR
static Value syn_if(Table *env, Value args)
{
    EXPECT(arity_range, 2, 3, args);

    Value cond = car(args), then = cadr(args);
    Value v = eval(env, cond);
    CHECK_ERROR(v);
    if (v != Qfalse)
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
        return runtime_error("unbound variable: %s", value_to_string(ident));
    return Qnil;
}

static Value syn_set(Table *env, Value ident, Value expr)
{
    EXPECT(type, TYPE_SYMBOL, ident);
    Value v = eval(env, expr);
    CHECK_ERROR(v);
    return iset(env, ident, v);
}

// 4.2. Derived expression types
// 4.2.1. Conditionals

static Value expect_list_head(Value v)
{
    EXPECT(type_or, TYPE_NULL, TYPE_PAIR, v);
    return Qfalse;
}

//PTR
static Value syn_and(Table *env, Value args)
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
static Value syn_or(Table *env, Value args)
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

static Value let(Table *env, Value var, Value bindings, Value body)
{
    EXPECT(list_head, bindings);
    Value params = Qnil, symargs = Qnil;
    Value r = transpose_2xn(bindings, &params, &symargs);
    CHECK_ERROR(r);
    Value args = map_eval(env, symargs);
    CHECK_ERROR(args);
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

// 5.2. Definitions
static Value define_variable(Table *env, Value ident, Value expr)
{
    EXPECT(type, TYPE_SYMBOL, ident);

    Value val = eval(env, expr);
    CHECK_ERROR(val);
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
        return runtime_error("wrong number of arguments: expected 1+");
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
    case TYPE_UNDEF:
        return runtime_error("the first argument expected symbol or pair but got %s",
                             value_type_to_string(t));
    }
    UNREACHABLE();
}

// 6.1. Equivalence predicates
static Value proc_eq(UNUSED Table *env, Value x, Value y)
{
    return OF_BOOL(x == y);
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
    EXPECT(type, TYPE_PAIR, pair);
    return car(pair);
}

static Value proc_cdr(UNUSED Table *env, Value pair)
{
    EXPECT(type, TYPE_PAIR, pair);
    return cdr(pair);
}

bool value_is_null(Value v)
{
    return v == Qnil;
}


static Value proc_list_p(UNUSED Table *env, Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        if (!value_is_pair(p))
            return Qfalse;
    }
    return Qtrue;
}

int64_t length(Value l)
{
    int64_t len = 0;
    for (Value p = l; p != Qnil; p = cdr(p))
        len++;
    return len;
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
    EXPECT(list_head, list);
    return reverse(list);
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

static Value build_apply_args(Value args)
{
    Value heads = DUMMY_PAIR(), p = args;
    for (Value last = heads, next; (next = cdr(p)) != Qnil; p = next)
        last = PAIR(last)->cdr = list1(car(p));
    Value rest = car(p);
    EXPECT(list_head, rest);
    return append2(cdr(heads), rest);
}

static Value proc_apply(Table *env, Value args)
{
    EXPECT(arity_min, 2, args);

    Value proc = car(args);
    EXPECT(type, TYPE_PROC, proc);
    Value appargs = build_apply_args(cdr(args));
    return apply(env, proc, appargs);
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

// 6.6.4. System interface
static Value proc_load(UNUSED Table *env, Value path)
{
    // Current spec: path is always relative
    return load_inner(value_to_string(path));
}

// Extensions from R7RS (scheme process-context)
static Value proc_exit(UNUSED Table *env, Value args)
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

static Value proc_cputime(UNUSED Table *env) // in micro sec
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

void sch_init(uintptr_t *sp)
{
    gc_init(sp);

    static char basedir[PATH_MAX];
    load_basedir = getcwd(basedir, sizeof(basedir));
#define DEF_SYMBOL(var, name) SYM_##var = value_of_symbol(name)
    DEF_SYMBOL(QUOTE, "quote");

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
    define_syntax(e, "and", syn_and, -1);
    define_syntax(e, "or", syn_or, -1);
    // 4.2.2. Binding constructs
    define_syntax(e, "let", syn_let, -1); // with named let in 4.2.4.

    // 5. Program structure

    // 5.2. Definitions
    define_syntax(e, "define", syn_define, -1);

    // 6. Standard procedures

    // 6.1. Equivalence predicates
    define_procedure(e, "eq?", proc_eq, 2);
    // 6.3.2. Pairs and lists
    define_procedure(e, "pair?", proc_pair_p, 1);
    define_procedure(e, "cons", proc_cons, 2);
    define_procedure(e, "car", proc_car, 1);
    define_procedure(e, "cdr", proc_cdr, 1);
    define_procedure(e, "list?", proc_list_p, 1);
    define_procedure(e, "reverse", proc_reverse, 1);
    // 6.4. Control features
    define_procedure(e, "apply", proc_apply, -1);
    // 6.6.3. Output
    define_procedure(e, "display", proc_display, 1);
    // 6.6.4. System interface
    define_procedure(e, "load", proc_load, 1);

    // Extensions from R7RS (scheme process-context)
    define_procedure(e, "exit", proc_exit, -1);

    // Local Extensions
    define_procedure(e, "print", proc_print, -1); // like Gauche
    define_procedure(e, "_cputime", proc_cputime, 0);
    define_syntax(e, "_defined?", syn_defined_p, 1);
}
