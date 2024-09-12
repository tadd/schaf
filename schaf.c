#include <errno.h>
#include <inttypes.h>
#include <math.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <libgen.h>
#include <limits.h>
#include <unistd.h>

#include "intern.h"
#include "libscary.h"
#include "schaf.h"
#include "utils.h"

//
// Types
//

// Value (uintptr_t):
//   0b.....000 Pointer (Unchangeable pattern!)
//   0b.......1 Integer
//   0b......10 Symbol
//   0b0--00100 #f
//   0b0--01100 #t
//   0b0-010100 <undef>
static const uintptr_t FLAG_NBIT_SYM = 2;
static const uintptr_t FLAG_NBIT_INT = 1;
static const uintptr_t FLAG_MASK     = 0b111; // for 64 bit machine
static const uintptr_t FLAG_MASK_SYM =  0b11;
static const uintptr_t FLAG_MASK_INT =   0b1;
static const uintptr_t FLAG_SYM      =  0b10;
static const uintptr_t FLAG_INT      =   0b1;
const Value SCH_NULL  = 0b11100U; // emtpy list
const Value SCH_FALSE = 0b00100U;
const Value SCH_TRUE  = 0b01100U;
const Value SCH_UNDEF = 0b10100U; // may be an error or something
#define BOOL_VAL(v) ((!!(v) << 3U) | 0b100U)

static const int64_t CFUNCARG_MAX = 3;

//
// Runtime-locals (aka global variables)
//

// Environment: list of Frames
// Frame: Table of 'symbol => <value>
static Value env_toplevel;
static char **symbol_names; // ("name0" "name1" ...)
Value SYM_QUOTE;
static const char *load_basedir;
static Source **source_data;
static jmp_buf jmp_exit;
static uint8_t exit_status; // should be <= 125 to be portable
static char errmsg[BUFSIZ];

// Singletons
static Value eof_object = Qfalse;
static Value current_input_port = Qfalse, current_output_port = Qfalse;
#define INIT_SINGLETON(var, val) do { if ((var) == Qfalse) (var) = val; } while (0)

//
// value_is_*: Type Checks
//

inline bool sch_value_is_integer(Value v)
{
    return v & FLAG_MASK_INT;
}

inline bool sch_value_is_symbol(Value v)
{
    return (v & FLAG_MASK_SYM) == FLAG_SYM;
}

static inline bool value_is_immediate(Value v)
{
    return v & FLAG_MASK;
}

static inline bool value_tag_is(Value v, ValueTag expected)
{
    return !value_is_immediate(v) && VALUE_TAG(v) == expected;
}

inline bool sch_value_is_string(Value v)
{
    return value_tag_is(v, TAG_STRING);
}

#define bug_invalid_tag(t) bug("got invalid tag %u", t)

inline bool sch_value_is_pair(Value v)
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
    if (sch_value_is_integer(v))
        return TYPE_INT;
    if (sch_value_is_symbol(v))
        return TYPE_SYMBOL;
    if (v == Qtrue || v == Qfalse)
        return TYPE_BOOL;
    if (v == Qundef)
        return TYPE_UNDEF;
    bug("unexpected immediate: %zu", v);
}

Type sch_value_type_of(Value v)
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
    case TAG_CFUNC_CLOSURE:
        return TYPE_PROC;
    case TAG_VECTOR:
        return TYPE_VECTOR;
    case TAG_ENV:
        return TYPE_ENV;
    case TAG_PORT:
        return TYPE_PORT;
    case TAG_PROMISE:
        return TYPE_PROMISE;
    case TAG_EOF:
        return TYPE_EOF;
    case TAG_ERROR:
        break; // internal objects
    }
    bug_invalid_tag(VALUE_TAG(v));
}

static const char *value_type_to_string(Type t)
{
    switch (t) {
    case TYPE_BOOL:
        return "boolean";
    case TYPE_INT:
        return "integer";
    case TYPE_SYMBOL:
        return "symbol";
    case TYPE_NULL:
        return "null";
    case TYPE_UNDEF:
        return "undef";
    case TYPE_PAIR:
        return "pair";
    case TYPE_STRING:
        return "string";
    case TYPE_PROC:
        return "procedure";
    case TYPE_VECTOR:
        return "vector";
    case TYPE_ENV:
        return "environment";
    case TYPE_PORT:
        return "port";
    case TYPE_PROMISE:
        return "promise";
    case TYPE_EOF:
        return "eof";
    }
    UNREACHABLE();
}

const char *sch_value_to_type_name(Value v)
{
    Type t = sch_value_type_of(v);
    return value_type_to_string(t);
}

// *_to_<cdata>: Convert internal data to external plain C

inline int64_t sch_integer_to_cint(Value x)
{
#if __x86_64__
    return (int64_t) x >> FLAG_NBIT_INT;
#else
    int64_t i = x;
    return (i - 1) / (1 << FLAG_NBIT_INT);
#endif
}

inline Symbol sch_symbol_to_csymbol(Value v)
{
    return (Symbol) v >> FLAG_NBIT_SYM;
}

// symbol->string
static const char *unintern(Symbol sym)
{
    size_t len = scary_length(symbol_names);
    if (UNLIKELY(sym >= len)) // fatal; every known symbols should have a name
        bug("symbol %lu not found", sym);
    return symbol_names[sym];
}

inline const char *sch_string_to_cstr(Value v)
{
    return STRING(v);
}

inline const char *sch_symbol_to_cstr(Value v)
{
    return unintern(sch_symbol_to_csymbol(v));
}

// define caar, cadr, ... cddddr, 28 procedures, at once
#define CXR1(f, x) f(a, x) f(d, x)
#define CXR2(f, x) CXR1(f, a ## x) CXR1(f, d ## x)
#define CXR3(f, x) CXR2(f, a ## x) CXR2(f, d ## x)
#define CXR4(f, x) CXR3(f, a) CXR3(f, d)
#define CXRS(f) CXR2(f,) CXR3(f,) //CXR4(f,)

#define DEF_CXR(x, y) \
    static Value c##x##y##r(Value v) { return c##x##r(c##y##r(v)); }
CXRS(DEF_CXR)

// *_new: Convert external plain C data to internal

inline Value sch_integer_new(int64_t i)
{
    Value v = i;
    return v << FLAG_NBIT_INT | FLAG_INT;
}

// string->symbol
static Symbol intern(const char *name)
{
    uint64_t i;
    size_t len = scary_length(symbol_names);
    // find
    for (i = 0; i < len; i++) {
        if (strcmp(symbol_names[i], name) == 0)
            return i;
    }
    // or put at `i`
    scary_push(&symbol_names, xstrdup(name));
    return i;
}

inline Value sch_symbol_new(const char *s)
{
    Symbol sym = intern(s);
    return (Value) (sym << FLAG_NBIT_SYM | FLAG_SYM);
}

void *obj_new(ValueTag t, size_t size)
{
    Header *h = gc_malloc(size);
    h->tag = t;
    h->immutable = false;
    return h;
}

static Value string_new_moved(char *str)
{ // move ownership then use as is
    String *s = obj_new(TAG_STRING, sizeof(String));
    STRING(s) = str;
    return (Value) s;
}

Value sch_string_new(const char *str)
{
    return string_new_moved(xstrdup(str));
}

static void expect_cfunc_tag(ValueTag tag)
{
    if (LIKELY(tag == TAG_CFUNC || tag == TAG_SYNTAX))
        return;
    bug_invalid_tag(tag);
}

static void expect_cfunc_arity(int64_t actual)
{
    if (LIKELY(actual <= CFUNCARG_MAX))
        return;
    bug("arity too large: expected ..%"PRId64" but got %"PRId64,
        CFUNCARG_MAX, actual);
}

// generic macros to handle errors and early-returns
#define CHECK_ERROR_F(v, f) do { \
        Value V = (v); \
        if (UNLIKELY(f(V))) \
            return V; \
    } while (0)
#define CHECK_ERROR(v) CHECK_ERROR_F(v, is_error)
#define TRUTHY(v) ((v) != Qfalse)
#define CHECK_ERROR_TRUTHY(v) CHECK_ERROR_F(v, TRUTHY)
#define CHECK_ERROR_LOCATED(v, l) do { \
        Value V = (v); \
        if (UNLIKELY(is_error(V))) { \
            if (scary_length(ERROR(V)) == 0) \
                push_stack_frame(V, NULL, (l)); \
            return V; \
        } \
    } while (0)
#define EXPECT(f, ...) CHECK_ERROR(expect_##f(__VA_ARGS__))

static Value expect_arity_0(Value args);
static Value expect_arity_1(Value args);
static Value expect_arity_2(Value args);
static Value expect_arity_3(Value args);

static Value apply_cfunc_v(Value env, Value f, Value args)
{
    return CFUNC(f)->f1(env, args);
}

static Value apply_cfunc_0(Value env, Value f, UNUSED Value args)
{
    EXPECT(arity_0, args);
    return CFUNC(f)->f0(env);
}

static Value apply_cfunc_1(Value env, Value f, Value args)
{
    EXPECT(arity_1, args);
    return CFUNC(f)->f1(env, car(args));
}

static Value apply_cfunc_2(Value env, Value f, Value args)
{
    EXPECT(arity_2, args);
    return CFUNC(f)->f2(env, car(args), cadr(args));
}

static Value apply_cfunc_3(Value env, Value f, Value args)
{
    EXPECT(arity_3, args);
    return CFUNC(f)->f3(env, car(args), cadr(args), caddr(args));
}

static Value cfunc_new_internal(ValueTag tag, const char *name, void *cfunc, int64_t arity)
{
    expect_cfunc_tag(tag);
    expect_cfunc_arity(arity);
    CFunc *f = obj_new(tag, sizeof(CFunc));
    f->proc.arity = arity;
    f->name = name;
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

static Value cfunc_new(const char *name, void *cfunc, int64_t arity)
{
    return cfunc_new_internal(TAG_CFUNC, name, cfunc, arity);
}

static Value syntax_new(const char *name, void *cfunc, int64_t arity)
{
    return cfunc_new_internal(TAG_SYNTAX, name, cfunc, arity);
}

static Value eval_body(Value env, Value body);
static Value env_inherit(Value parent);
static Value env_put(Value env, Value key, Value value);
static Value expect_arity(int64_t expected, Value args);

//PTR
static Value apply_closure(UNUSED Value env, Value proc, Value args)
{
    EXPECT(arity, PROCEDURE(proc)->arity, args);
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

static Value closure_new(Value env, Value params, Value body)
{
    Closure *f = obj_new(TAG_CLOSURE, sizeof(Closure));
    bool headp = params == Qnil || sch_value_is_pair(params);
    f->proc.arity = headp ? length(params) : -1;
    f->proc.apply = apply_closure;
    f->env = env;
    f->params = params;
    f->body = body;
    return (Value) f;
}

// and `cons` is well-known name than `pair_new`

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

// error_new() or
static Value runtime_error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(errmsg, sizeof(errmsg), fmt, ap);
    va_end(ap);
    Error *e = obj_new(TAG_ERROR, sizeof(Error));
    ERROR(e) = scary_new(sizeof(StackFrame *));
    return (Value) e;
}

const char *sch_error_message(void)
{
    return errmsg;
}

static Value expect_type(Type expected, Value v)
{
    Type t = sch_value_type_of(v);
    if (LIKELY(t == expected))
        return Qfalse;
    return runtime_error("expected %s but got %s",
                         value_type_to_string(expected), value_type_to_string(t));
}

static Value expect_type_or(Type e1, Type e2, Value v)
{
    Type t = sch_value_type_of(v);
    if (LIKELY(t == e1 || t == e2))
        return Qfalse;
    return runtime_error("expected %s or %s but got %s",
                         value_type_to_string(e1), value_type_to_string(e2),
                         value_type_to_string(t));
}

static bool length_in_range(Value l, int64_t min, int64_t max)
{
    int64_t len = 0;
    for (Value p = l; p != Qnil; p = cdr(p)) {
        len++;
        if (len > max)
            return false;
    }
    return len >= min;
}

static Value arity_error(const char *op, int64_t expected, int64_t actual)
{
    return runtime_error("wrong number of arguments: expected %s%d but got %d",
                         op, expected, actual);
}

static Value expect_arity_range(int64_t min, int64_t max, Value args)
{
    if (LIKELY(length_in_range(args, min, max)))
        return Qfalse;
    return runtime_error("wrong number of arguments: expected %"PRId64"..%"PRId64
                         " but got %"PRId64, min, max, length(args));
}

static Value expect_arity(int64_t expected, Value args)
{
    if (LIKELY(expected < 0 || length_in_range(args, expected, expected)))
        return Qfalse;
    return arity_error("", expected, length(args));
}

static Value expect_arity_min_2(Value args)
{
    if (LIKELY(args != Qnil && cdr(args) != Qnil))
        return Qfalse;
    return arity_error(">= ", 2, length(args));
}

static Value expect_arity_0(Value args)
{
    if (LIKELY(args == Qnil))
        return Qfalse;
    return arity_error("", 0, length(args));
}

static Value expect_arity_1(Value args)
{
    if (LIKELY(args != Qnil && cdr(args) == Qnil))
        return Qfalse;
    return arity_error("", 1, length(args));
}

static Value expect_arity_2(Value args)
{
    if (LIKELY(args != Qnil && cdr(args) != Qnil && cddr(args) == Qnil))
        return Qfalse;
    return arity_error("", 2, length(args));
}

static Value expect_arity_3(Value args)
{
    if (LIKELY(args != Qnil && cdr(args) != Qnil && cddr(args) != Qnil && cdddr(args) == Qnil))
        return Qfalse;
    return arity_error("", 3, length(args));
}

[[gnu::nonnull(1)]]
static Value env_new(const char *name)
{
    Env *e = obj_new(TAG_ENV, sizeof(Env));
    e->table = NULL;
    e->parent = Qfalse;
    e->name = name;
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
    Table *t = ENV(env)->table;
    if (t == NULL)
        t = ENV(env)->table = table_new();
    table_put(t, SYMBOL(key), value);
    return env;
}

// chained!
static bool env_set(Value env, Value key, Value value)
{
    Symbol sym = SYMBOL(key);
    for (Value p = env; p != Qfalse; p = ENV(p)->parent) {
        Table *t = ENV(p)->table;
        if (t == NULL)
            continue;
        if (table_set(t, sym, value))
            return true;
    }
    return false;
}

// chained!!
static Value env_get(const Value env, Value name)
{
    Symbol sym = SYMBOL(name);
    for (Value p = env; p != Qfalse; p = ENV(p)->parent) {
        Table *t = ENV(p)->table;
        if (t == NULL)
            continue;
        Value v = table_get(t, sym);
        if (v != TABLE_NOT_FOUND)
            return v;
    }
    return Qundef;
}

// Note: Do not mistake this for "(define-syntax ...)" which related to macros
static void define_syntax(Value env, const char *name, void *cfunc, int64_t arity)
{
    env_put(env, sch_symbol_new(name), syntax_new(name, cfunc, arity));
}

static void define_procedure(Value env, const char *name, void *cfunc, int64_t arity)
{
    env_put(env, sch_symbol_new(name), cfunc_new(name, cfunc, arity));
}

//
// Evaluation
//
static Value eval(Value env, Value v);

static Value eval_body(Value env, Value body)
{
    Value last = Qfalse;
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

static StackFrame *stack_frame_new(const char *name, Value loc)
{
    StackFrame *f = xmalloc(sizeof(StackFrame));
    f->func_name = name;
    f->loc = loc;
    return f;
}

static Value push_stack_frame(Value ve, const char *name, Value loc)
{
    StackFrame *f = stack_frame_new(name, loc);
    scary_push((void ***) &ERROR(ve), (const void *) f);
    return ve;
}

static Value apply(Value env, Value proc, Value args)
{
    return PROCEDURE(proc)->apply(env, proc, args);
}

static const char *get_func_name(Value proc)
{
    ValueTag t = VALUE_TAG(proc);
    if (t == TAG_CFUNC || t == TAG_SYNTAX)
        return CFUNC(proc)->name;
    return NULL;
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
        const char *fname = get_func_name(proc);
        return push_stack_frame(ret, fname, l);
    }
    return ret;
}

static Value lookup_or_error(Value env, Value v)
{
    Value p = env_get(env, v);
    if (p == Qundef)
        return runtime_error("unbound variable: %s", sch_symbol_to_cstr(v));
    return p;
}

static Value eval(Value env, Value v)
{
    if (sch_value_is_symbol(v))
        return lookup_or_error(env, v);
    if (v == Qnil || !sch_value_is_pair(v))
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

static const int64_t *filename_to_newline_pos(const char *filename)
{
    for (size_t i = 0, len = scary_length(source_data); i < len; i++) {
        if (strcmp(source_data[i]->filename, filename) == 0)
            return source_data[i]->newline_pos;
    }
    return NULL;
}

static void frame_to_line_col(const StackFrame *f, const int64_t *newline_pos,
                              int64_t *line, int64_t *col)
{
    pos_to_line_col(LOCATED_PAIR(f->loc)->pos, newline_pos, line, col);
}

static void dump_line_column(const char *filename, const StackFrame *f)
{
    int64_t line, col;
    const int64_t *newline_pos = filename_to_newline_pos(filename);
    if (newline_pos == NULL) {
        append_error_message("\n\t<unknown>");
        return;
    }
    frame_to_line_col(f, newline_pos, &line, &col);
    append_error_message("\n\t%s:%"PRId64":%"PRId64" in ",
                         filename, line, col);
}

static bool find_pair(Value tree, Value pair)
{
    for (Value p = tree; p != Qnil; p = cdr(p)) {
        if (p == pair)
            return true;
        if (!sch_value_is_pair(p))
            return false;
        Value v = car(p);
        if (sch_value_is_pair(v) && find_pair(v, pair))
            return true;
    }
    return false;
}

static const char *find_filename(Value pair)
{
    for (size_t i = 0, len = scary_length(source_data); i < len; i++) {
        Source *src = source_data[i];
        if (find_pair(src->ast, pair))
            return src->filename;
    }
    return NULL;
}

static void dump_callee_name(const StackFrame *next)
{
    if (next == NULL) {
        append_error_message("<toplevel>");
        return;
    }
    Value sym = car(next->loc);
    if (!sch_value_is_symbol(sym)) {
        append_error_message("<unknown>");
        return;
    }
    const char *name = sch_symbol_to_cstr(sym);
    append_error_message("'%s'", name);
}

static void dump_frame(const StackFrame *f, const StackFrame *next)
{
    const char *filename = find_filename(f->loc);
    if (filename == NULL) {
        append_error_message("\n\t<unknown>");
        return;
    }
    dump_line_column(filename, f);
    dump_callee_name(next);
}

static void prepend_cfunc_name(const char *name)
{
    if (name == NULL)
        return;
    size_t len = strlen(name) + 2;// strlen(": ")
    char tmp[sizeof(errmsg) - len];
    snprintf(tmp, sizeof(tmp), "%s", errmsg);
    snprintf(errmsg, sizeof(errmsg), "%s: %s", name, tmp);
}

static bool ignore_lines(FILE *f, int64_t n)
{
    char buf[BUFSIZ];
    for (ssize_t i = 0; i < n; i++) {
        if (fgets(buf, sizeof(buf), f) == NULL)
            return false;
    }
    return true;
}

static bool dump_nth_line(const char *filename, int64_t n)
{
    FILE *f = fopen(filename, "r");
    if (f == NULL)
        return false;
    char buf[BUFSIZ];
    bool ret = false;
    if (!ignore_lines(f, n-1) ||
        fgets(buf, sizeof(buf), f) == NULL)
        goto out;
    append_error_message("\n%s", buf); // `buf` includes "\n"
    ret = true;
 out:
    fclose(f);
    return ret;
}

// <space><space>...^
static void dump_column_point(int64_t col)
{
    for (ssize_t i = 1; i < col; i++)
        append_error_message(" ");
    append_error_message("^");
}

static void dump_error_line_with_point(const StackFrame *f)
{
    const char *filename = find_filename(f->loc);
    if (filename == NULL)
        return;
    const int64_t *newline_pos = filename_to_newline_pos(filename);
    if (newline_pos == NULL)
        return;
    int64_t line, col;
    frame_to_line_col(f, newline_pos, &line, &col);
    if (dump_nth_line(filename, line))
        dump_column_point(col);
}

static void dump_stack_trace(StackFrame *const *call_stack)
{
    size_t len = scary_length(call_stack);
    if (len == 0)
        return;
    const StackFrame *top = call_stack[0];
    prepend_cfunc_name(top->func_name);
    dump_error_line_with_point(top);
    for (size_t i = 0; i < len - 1; i++)
        dump_frame(call_stack[i], call_stack[i+1]);
    dump_frame(call_stack[len-1], NULL);
}

static void add_source(Source ***psource_data, const Source *newsrc)
{
    scary_push((void ***) psource_data, (const void *) newsrc);
    Source **data = *psource_data;
    size_t len = scary_length(data);
    gc_add_root(&data[len-1]->ast);
}

static Value iload(FILE *in, const char *filename)
{
    Source *src = iparse(in, filename);
    if (src == NULL)
        return Qundef;
    add_source(&source_data, src);
    if (setjmp(jmp_exit) != 0)
        return sch_integer_new(exit_status);
    Value ret = eval_body(env_toplevel, src->ast);
    if (is_error(ret)) {
        dump_stack_trace(ERROR(ret));
        return Qundef;
    }
    return ret;
}

static Value iload_inner(FILE *in, const char *path)
{
    Source *src = iparse(in, path);
    if (src == NULL)
        return Qundef;
    add_source(&source_data, src);
    return eval_body(env_toplevel, src->ast);
}

Value sch_eval_string(const char *in)
{
    FILE *f = mopen(in);
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

Value sch_load(const char *path)
{
    FILE *in = open_loadable(path);
    if (UNLIKELY(in == NULL))
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
        return runtime_error("can't open file: %s", path);
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
    return closure_new(env, params, body);
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
        return runtime_error("unbound variable: %s", sch_symbol_to_cstr(ident));
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

static Value expect_list_head(Value v)
{
    EXPECT(type_or, TYPE_NULL, TYPE_PAIR, v);
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
static Value let(Value env, Value var, Value bindings, Value body)
{
    EXPECT(list_head, bindings);
    bool named = var != Qfalse;
    Value letenv = env_inherit(env);
    Value params = DUMMY_PAIR(), lparams = params;
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        Value b = car(p);
        EXPECT(type, TYPE_PAIR, b);
        if (length(b) != 2)
            return runtime_error("malformed binding: %s", sch_stringify(b));
        Value ident = car(b), expr = cadr(b);
        EXPECT(type, TYPE_SYMBOL, ident);
        if (named)
            lparams = PAIR(lparams)->cdr = list1(ident);
        Value val = eval(env, expr);
        CHECK_ERROR(val);
        env_put(letenv, ident, val);
    }
    if (named) {
        Value proc = closure_new(letenv, cdr(params), body);
        env_put(letenv, var, proc); // letenv affects as proc->env
    }
    return eval_body(letenv, body);
}

//PTR
static Value syn_let(Value env, Value args)
{
    EXPECT(arity_min_2, args);
    Value bind_or_var = car(args), body = cdr(args);
    Value var = Qfalse, bindings = bind_or_var;
    if (sch_value_is_symbol(bind_or_var)) {
        var = bind_or_var;
        bindings = car(body);
        body = cdr(body);
    }
    return let(env, var, bindings, body);
}

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
    Value val = closure_new(env, params, body);
    return define_variable(env, ident, val);
}

static Value syn_define(Value env, Value args)
{
    if (args == Qnil)
        return arity_error(">= ", 1, 0);
    Value head = car(args);
    Type t = sch_value_type_of(head);
    switch (t) {
    case TYPE_SYMBOL:
        EXPECT(arity_2, args);
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
    case TYPE_PROMISE:
    case TYPE_EOF:
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
    return BOOL_VAL(x == y);
}

static bool equal(Value x, Value y);

static bool vector_equal(const Value *x, const Value *y)
{
    size_t len = scary_length(x);
    if (len != scary_length(y))
        return false;
    for (size_t i = 0; i < len; i++) {
        if (!equal(x[i], y[i]))
            return false;
    }
    return true;
}

static bool equal(Value x, Value y)
{
    if (x == y)
        return true;
    Type tx = sch_value_type_of(x), ty = sch_value_type_of(y);
    if (tx != ty)
        return false;
    switch (tx) {
    case TYPE_PAIR:
        return equal(car(x), car(y)) &&
               equal(cdr(x), cdr(y));
    case TYPE_STRING:
        return strcmp(STRING(x), STRING(y)) == 0;
    case TYPE_VECTOR:
        return vector_equal(VECTOR(x), VECTOR(y));
    case TYPE_SYMBOL:
    case TYPE_NULL:
    case TYPE_BOOL:
    case TYPE_INT:
    case TYPE_PROC:
    case TYPE_ENV:
    case TYPE_PORT:
    case TYPE_PROMISE:
    case TYPE_EOF:
    case TYPE_UNDEF:
        return false;
    }
    UNREACHABLE();
}

// 6.3.2. Pairs and lists
static Value proc_pair_p(UNUSED Value env, Value o)
{
    return BOOL_VAL(sch_value_is_pair(o));
}

Value cons(Value car, Value cdr)
{
    Pair *p = obj_new(TAG_PAIR, sizeof(Pair));
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

static Value proc_null_p(UNUSED Value env, Value list)
{
    return BOOL_VAL(list == Qnil);
}

static Value proc_list_p(UNUSED Value env, Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        if (!sch_value_is_pair(p))
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

static Value reverse(Value l)
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
    EXPECT(arity_min_2, args);

    Value proc = car(args);
    EXPECT(type, TYPE_PROC, proc);
    Value appargs = build_apply_args(cdr(args));
    CHECK_ERROR(appargs);
    return apply(env, proc, appargs);
}

// 6.6.3. Output
static const char *file_to_name(const FILE *fp)
{
    return fp == stdin ? "stdin" :
        fp == stdout ? "stdout" :
        fp == stderr ? "stderr" :
        NULL;
}

static void display_port(FILE *f, const FILE *fp)
{
    const char *name = file_to_name(fp);
    if (name != NULL)
        fprintf(f, "<port: %s>", name);
    else
        fprintf(f, "<port: %p>", fp);
}

static void display_procedure(FILE *f, Value proc)
{
    const char *name = get_func_name(proc);
    if (name != NULL)
        fprintf(f, "<procedure: %s>", name);
    else
        fprintf(f, "<procedure>");
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

static bool check_circular(FILE *f, Value p, Value record)
{
    bool circular = memq(p, record) != Qfalse;
    if (circular)
        fprintf(f, "..");
    return circular;
}

typedef void (*ValuePrinter)(FILE *f, Value v);
static void print_object(FILE *f, Value v, Value record, ValuePrinter printer);

static void print_pair(FILE *f, Value l, Value record, ValuePrinter printer)
{
    fprintf(f, "(");
    for (Value p = l, next; p != Qnil; p = next) {
        if (check_circular(f, p, record))
            break;
        record = cons(p, record);
        Value val = car(p);
        if (!check_circular(f, val, record))
            print_object(f, val, record, printer);
        next = cdr(p);
        if (next == Qnil)
            break;
        fprintf(f, " ");
        if (!sch_value_is_pair(next)) {
            fprintf(f, ". ");
            print_object(f, next, record, printer);
            break;
        }
    }
    fprintf(f, ")");
}

static void print_vector(FILE *f, Value val, Value record, ValuePrinter printer)
{
    if (check_circular(f, val, record))
        goto end;
    fprintf(f, "#(");
    record = cons(val, record);
    const Value *v = VECTOR(val);
    for (int64_t i = 0, len = scary_length(v); i < len; i++) {
        Value e = v[i];
        if (!check_circular(f, e, record))
            print_object(f, e, record, printer);
        if (i + 1 < len)
            fprintf(f, " ");
    }
 end:
    fprintf(f, ")");
}

static void print_object(FILE *f, Value v, Value record, ValuePrinter printer)
{
    switch (sch_value_type_of(v)) {
    case TYPE_SYMBOL:
    case TYPE_STRING:
    case TYPE_NULL:
    case TYPE_BOOL:
    case TYPE_INT:
    case TYPE_PROC:
    case TYPE_UNDEF:
    case TYPE_ENV:
    case TYPE_PORT:
    case TYPE_PROMISE:
    case TYPE_EOF:
        printer(f, v);
        break;
    case TYPE_PAIR:
        print_pair(f, v, record, printer);
        break;
    case TYPE_VECTOR:
        print_vector(f, v, record, printer);
        break;
    }
}

static void fdisplay_single(FILE *f, Value v)
{
    switch (sch_value_type_of(v)) {
    case TYPE_NULL:
        fprintf(f, "()");
        break;
    case TYPE_BOOL:
        fprintf(f, "%s", v == Qtrue ? "#t" : "#f");
        break;
    case TYPE_INT:
        fprintf(f, "%"PRId64, INT(v));
        break;
    case TYPE_SYMBOL:
        fprintf(f, "%s", sch_symbol_to_cstr(v));
        break;
    case TYPE_STRING:
        fprintf(f, "%s", STRING(v));
        break;
    case TYPE_PROC:
        display_procedure(f, v);
        break;
    case TYPE_ENV:
        fprintf(f, "<environment: %s>", ENV(v)->name);
        break;
    case TYPE_PORT:
        display_port(f, PORT(v)->fp);
        break;
    case TYPE_PROMISE:
        fprintf(f, "<promise: %p>", (void *) v);
        break;
    case TYPE_EOF:
        fprintf(f, "<eof>");
        break;
    case TYPE_UNDEF:
        fprintf(f, "<undef>");
        break;
    case TYPE_PAIR:
    case TYPE_VECTOR:
        bug("invalid type %s", sch_value_to_type_name(v));
    }
}

static void fdisplay(FILE *f, Value v)
{
    print_object(f, v, Qnil, fdisplay_single);
}

char *sch_stringify(Value v)
{
    char *s;
    errno = 0;
    FILE *fp = mopen_w(&s);
    fdisplay(fp, v);
    fclose(fp);
    return s;
}

void sch_display(Value v)
{
    fdisplay(stdout, v);
}

static Value port_new(FILE *fp, PortType type)
{
    Port *p = obj_new(TAG_PORT, sizeof(Port));
    p->fp = fp;
    p->type = type;
    p->string = NULL;
    return (Value) p;
}

static Value get_current_input_port(void)
{
    INIT_SINGLETON(current_input_port, port_new(stdin, PORT_INPUT));
    return current_input_port;
}

static Value get_current_output_port(void)
{
    INIT_SINGLETON(current_output_port, port_new(stdout, PORT_OUTPUT));
    return current_output_port;
}

static Value arg_or_current_port(Value arg, PortType type)
{
    if (arg == Qnil)
        return type == PORT_OUTPUT ? get_current_output_port() : get_current_input_port();
    Value p = car(arg);
    EXPECT(type, TYPE_PORT, p);
    return p;
}

static Value proc_display(UNUSED Value env, Value args)
{
    EXPECT(arity_range, 1, 2, args);
    Value obj = car(args);
    Value out = arg_or_current_port(cdr(args), PORT_OUTPUT);
    CHECK_ERROR(out);
    fdisplay(PORT(out)->fp, obj);
    return Qfalse;
}

// 6.6.4. System interface
static Value proc_load(UNUSED Value env, Value path)
{
    EXPECT(type, TYPE_STRING, path);
    // Current spec: path is always relative
    return load_inner(STRING(path));
}

//
// Interpreter things
//

#define scary_free_with(f, a) do { \
        for (size_t i = 0, len = scary_length(a); i < len; i++) \
            f(a[i]); \
        scary_free(a); \
    } while (0)

int sch_fin(void)
{
    scary_free_with(source_free, source_data);
    scary_free_with(free, symbol_names);
    gc_fin();
    return exit_status;
}

void sch_init(const uintptr_t *sp)
{
    gc_init(sp);

    gc_add_root(&eof_object);

    static char basedir[PATH_MAX];
    load_basedir = getcwd(basedir, sizeof(basedir));
    symbol_names = scary_new(sizeof(char *));
#define DEF_SYMBOL(var, name) SYM_##var = sch_symbol_new(name)
    DEF_SYMBOL(QUOTE, "quote");

    env_toplevel = env_new("default");
    gc_add_root(&env_toplevel);
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
    define_syntax(e, "and", syn_and, -1);
    define_syntax(e, "or", syn_or, -1);
    // 4.2.2. Binding constructs
    define_syntax(e, "let", syn_let, -1); // with named let in 4.2.4.
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
    define_procedure(e, "eq?", proc_eq, 2);
    // 6.3.2. Pairs and lists
    define_procedure(e, "pair?", proc_pair_p, 1);
    define_procedure(e, "cons", proc_cons, 2);
    define_procedure(e, "car", proc_car, 1);
    define_procedure(e, "cdr", proc_cdr, 1);
    define_procedure(e, "null?", proc_null_p, 1);
    define_procedure(e, "list?", proc_list_p, 1);
    define_procedure(e, "reverse", proc_reverse, 1);
    // 6.4. Control features
    define_procedure(e, "apply", proc_apply, -1);
    // 6.6.3. Output
    define_procedure(e, "display", proc_display, -1);
    // 6.6.4. System interface
    define_procedure(e, "load", proc_load, 1);
}
