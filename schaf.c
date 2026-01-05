#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <math.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <libgen.h>
#include <limits.h>
#include <sys/select.h>
#include <unistd.h>

#include "intern.h"
#include "libscary.h"
#include "schaf.h"
#include "utils.h"

//
// Types
//

// Value (uintptr_t):
//   0b............000 Pointer (Unchangeable pattern!)
//   0b..............1 Integer
//   0b.............10 Symbol
//   0b0-0{8bits!}0100 Character
//   0b0--------001100 #f
//   0b0--------011100 #t
//   0b0-------0101100 null
//   0b0-------0111100 <undef>
static const uintptr_t FLAG_NBIT_INT  = 1;
static const uintptr_t FLAG_NBIT_SYM  = 2;
static const uintptr_t FLAG_NBIT_CHAR = 4;
static const uintptr_t FLAG_MASK_INT  =    0b1;
static const uintptr_t FLAG_MASK_SYM  =   0b11;
static const uintptr_t FLAG_MASK_IMM  =  0b111; // for 64 bit machine
static const uintptr_t FLAG_MASK_CHAR = 0b1111;
static const uintptr_t FLAG_INT       =    0b1;
static const uintptr_t FLAG_SYM       =   0b10;
static const uintptr_t FLAG_CHAR      = 0b0100;
const Value SCH_FALSE = 0b001100U;
const Value SCH_TRUE  = 0b011100U;
const Value SCH_NULL  = 0b101100U; // emtpy list
const Value SCH_UNDEF = 0b111100U; // may be an error or something internal
#define BOOL_VAL(v) ((!!(v) << 4U) | 0b1100U)

static const int64_t CFUNCARG_MAX = 3;

//
// Runtime-locals (aka global variables)
//

// Environment: list of Frames
// Frame: Table of 'symbol => <value>
static Value env_toplevel, env_default, env_r5rs, env_null;
static char **symbol_names; // ("name0" "name1" ...)
Value SYM_QUOTE, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING;
static Value SYM_ELSE, SYM_RARROW;
static const char *load_basedir;
static Source **source_data;
static jmp_buf jmp_exit;
static uint8_t exit_status; // should be <= 125 to be portable
static char errmsg[BUFSIZ];
static Value inner_winders = Qnil; // both inner_* are used in (dynamic-wind)
static Value inner_continuation = Qfalse;

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
    return v & FLAG_MASK_IMM;
}

static inline bool value_tag_is(Value v, ValueTag expected)
{
    return !value_is_immediate(v) && VALUE_TAG(v) == expected;
}

inline bool sch_value_is_character(Value v)
{
    return (v & FLAG_MASK_CHAR) == FLAG_CHAR;
}

inline bool sch_value_is_string(Value v)
{
    return value_tag_is(v, TAG_STRING);
}

#define bug_invalid_tag(t) bug("got invalid tag %u", t)

static bool sch_value_is_procedure(Value v)
{
    if (value_is_immediate(v))
        return false;
    switch (VALUE_TAG(v)) {
    case TAG_SYNTAX:
    case TAG_CFUNC:
    case TAG_CLOSURE:
    case TAG_CONTINUATION:
    case TAG_CFUNC_CLOSURE:
        return true;
    case TAG_STRING:
    case TAG_PAIR:
    case TAG_VECTOR:
    case TAG_ENV:
    case TAG_PORT:
    case TAG_PROMISE:
    case TAG_EOF:
        return false;
    case TAG_ERROR:
        break; // internal objects
    }
    bug_invalid_tag(VALUE_TAG(v));
}

inline static bool sch_value_is_vector(Value v)
{
    return value_tag_is(v, TAG_VECTOR);
}

inline static bool sch_value_is_env(Value v)
{
    return value_tag_is(v, TAG_ENV);
}

inline static bool sch_value_is_port(Value v)
{
    return value_tag_is(v, TAG_PORT);
}

inline bool sch_value_is_pair(Value v)
{
    return value_tag_is(v, TAG_PAIR);
}

inline static bool sch_value_is_promise(Value v)
{
    return value_tag_is(v, TAG_PROMISE);
}

inline static bool is_error(Value v)
{
    return value_tag_is(v, TAG_ERROR);
}

inline static bool is_boolean(Value v)
{
    return v == Qtrue || v == Qfalse;
}

static Type immediate_type_of(Value v)
{
    if (sch_value_is_integer(v))
        return TYPE_INT;
    if (sch_value_is_symbol(v))
        return TYPE_SYMBOL;
    if (sch_value_is_character(v))
        return TYPE_CHAR;
    if (is_boolean(v))
        return TYPE_BOOL;
    if (v == Qnil)
        return TYPE_NULL;
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
    case TYPE_CHAR:
        return "character";
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
#ifdef __x86_64__
    return ((int64_t) x) >> FLAG_NBIT_INT;
#else
    int64_t i = x;
    return (i - 1) / (1 << FLAG_NBIT_INT);
#endif
}

inline Symbol sch_symbol_to_csymbol(Value v)
{
    return (Symbol) (v >> FLAG_NBIT_SYM);
}

inline uint8_t sch_character_to_uint8(Value v)
{
    return (uint8_t) (v >> FLAG_NBIT_CHAR);
}

// symbol->string
static const char *unintern(Symbol sym)
{
    size_t len = scary_length(symbol_names);
    if (sym >= len) // fatal; every known symbols should have a name
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
#define CXRS(f) CXR2(f,) CXR3(f,) CXR4(f,)

#define DEF_CXR(x, y) \
    UNUSED static Value c##x##y##r(Value v) { return c##x##r(c##y##r(v)); }
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
    return (((Value) sym) << FLAG_NBIT_SYM) | FLAG_SYM;
}

void *obj_new(ValueTag t, size_t size)
{
    Header *h = gc_malloc(size);
    h->tag = t;
    h->immutable = false;
    return h;
}

Value sch_character_new(uint8_t ch)
{
    return ((Value) ch) << FLAG_NBIT_CHAR | FLAG_CHAR;
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

// General macros for error handling
static Value push_stack_frame(Value ve, const char *name, Value loc);
#define EXPECT_OR_RETURN(expr, err) do { if (!(expr)) return err; } while (0)
#define EXPECT_ERROR_WITH_RETVAL(v, val) EXPECT_OR_RETURN(!is_error(v), (val))
#define EXPECT_ERROR(v) EXPECT_ERROR_WITH_RETVAL((v), (v))
#define EXPECT_ERROR_LOCATED(v, l) EXPECT_ERROR_WITH_RETVAL((v), ({ \
            if (scary_length(ERROR(v)) == 0) \
                push_stack_frame((v), NULL, (l)); \
            (v); \
        }))
// Error happened at the n-th argument in the caller (list)
#define EXPECT_ERROR_LOCATED_BY_CALLER(v, n) \
    EXPECT_ERROR_LOCATED(v, sch_integer_new(n))

static inline bool is_length_min_2(Value args)
{
    return args != Qnil && cdr(args) != Qnil;
}

static inline bool is_length_2(Value args)
{
    return is_length_min_2(args) && cddr(args) == Qnil;
}

static inline bool is_length_3(Value args)
{
    return is_length_min_2(args) && cddr(args) != Qnil && cdddr(args) == Qnil;
}

static Value arity_error(const char *op, int64_t expected, int64_t actual);

#define EXPECT_ARITY_N(expr, op, exp, act) \
    EXPECT_OR_RETURN((expr), arity_error((op), (exp), (act)))
#define EXPECT_ARITY_0(args) \
    EXPECT_ARITY_N((args) == Qnil, "", 0, length(args))
#define EXPECT_ARITY_1(args) \
    EXPECT_ARITY_N((args) != Qnil && cdr(args) == Qnil, "", 1, length(args))
#define EXPECT_ARITY_2(args) \
    EXPECT_ARITY_N(is_length_2(args), "", 2, length(args))
#define EXPECT_ARITY_3(args) \
    EXPECT_ARITY_N(is_length_3(args), "", 3, length(args))

static Value apply_cfunc_v(Value env, Value f, Value args)
{
    return CFUNC(f)->f1(env, args);
}

static Value apply_cfunc_0(Value env, Value f, UNUSED Value args)
{
    EXPECT_ARITY_0(args);
    return CFUNC(f)->f0(env);
}

static Value apply_cfunc_1(Value env, Value f, Value args)
{
    EXPECT_ARITY_1(args);
    return CFUNC(f)->f1(env, car(args));
}

static Value apply_cfunc_2(Value env, Value f, Value args)
{
    EXPECT_ARITY_2(args);
    return CFUNC(f)->f2(env, car(args), cadr(args));
}

static Value apply_cfunc_3(Value env, Value f, Value args)
{
    EXPECT_ARITY_3(args);
    return CFUNC(f)->f3(env, car(args), cadr(args), caddr(args));
}

static void expect_cfunc_tag(ValueTag tag)
{
    if (tag == TAG_CFUNC || tag == TAG_SYNTAX)
        return;
    bug_invalid_tag(tag);
}

static void expect_cfunc_arity(int64_t actual)
{
    if (actual <= CFUNCARG_MAX)
        return;
    bug("arity too large: expected ..%"PRId64" but got %"PRId64,
        CFUNCARG_MAX, actual);
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

static Value apply_cfunc_closure_1(UNUSED Value env, Value f, Value args)
{
    EXPECT_ARITY_1(args);
    return CFUNC(f)->f1(CFUNC_CLOSURE(f)->data, car(args));
}

static Value cfunc_closure_new(const char *name, void *cfunc, Value data)
{
    CFuncClosure *cc = obj_new(TAG_CFUNC_CLOSURE, sizeof(CFuncClosure));
    cc->data = data;
    CFunc *f = CFUNC(cc);
    f->proc.arity = 1; // currently fixed
    f->proc.apply = apply_cfunc_closure_1;
    f->name = name;
    f->cfunc = cfunc;
    return (Value) cc;
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

#define EXPECT_ARITY(expected, args) \
    EXPECT_ARITY_N(expected < 0 || length_in_range(args, expected, expected), \
                   "", expected, length(args))

static Value eval_body(Value env, Value body);
static Value env_inherit(Value parent);
static void env_put(Value env, Value key, Value value);

//PTR
static Value apply_closure(UNUSED Value env, Value proc, Value args)
{
    EXPECT_ARITY(PROCEDURE(proc)->arity, args);
    Closure *cl = CLOSURE(proc);
    int64_t arity = cl->proc.arity;
    Value localenv = env_inherit(cl->env);
    Value params = cl->params;
    if (arity == -1)
        env_put(localenv, params, args);
    else {
        for (Value pa = args, pp = params; pa != Qnil; pa = cdr(pa), pp = cdr(pp))
            env_put(localenv, car(pp), car(pa));
    }
    return eval_body(localenv, cl->body);
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

void print_error_message(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(errmsg, sizeof(errmsg), fmt, ap);
    va_end(ap);
}

void append_error_message_v(const char *fmt, va_list ap)
{
    size_t len = strlen(errmsg);
    ssize_t rest = sizeof(errmsg) - len;
    vsnprintf(errmsg + len, rest, fmt, ap);
}

[[gnu::format(printf, 1, 2)]]
static void append_error_message(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    append_error_message_v(fmt, ap);
    va_end(ap);
}

[[gnu::format(printf, 1, 2)]]
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

static Value runtime_error_with_obj(const char *message, Value obj)
{
    char *objstr = sch_stringify(obj);
    Value e = runtime_error("%s: %s", message, objstr);
    free(objstr);
    return e;
}

const char *sch_error_message(void)
{
    return errmsg;
}

static inline Value arity_error(const char *op, int64_t expected, int64_t actual)
{
    return runtime_error("wrong number of arguments: expected %s%"PRId64" but got %"PRId64,
                         op, expected, actual);
}

static inline Value type_error(const char *expected, Value v)
{
    return runtime_error("expected %s but got %s",
                         expected, sch_value_to_type_name(v));
}

#define EXPECT(expr, ...) EXPECT_OR_RETURN((expr), runtime_error(__VA_ARGS__))
#define EXPECT_WITH_OBJ(expr, ...) EXPECT_OR_RETURN((expr), runtime_error_with_obj(__VA_ARGS__))

#define EXPECT_TYPE(type, v) \
    EXPECT_OR_RETURN(sch_value_is_ ## type(v), type_error(#type, v))
#define EXPECT_TYPE_TWIN(type, x, y) EXPECT_TYPE(type, x); EXPECT_TYPE(type, y)
#define EXPECT_TYPE_OR(t1, t2, v) \
    EXPECT(sch_value_is_ ## t1(v) || sch_value_is_ ## t2(v), \
           "expected " #t1 " or " #t2 " but got %s", sch_value_to_type_name(v))

#define EXPECT_ARITY_RANGE(min, max, args) \
    EXPECT(length_in_range(args, min, max), \
           "wrong number of arguments: expected %d..%d but got %"PRId64, \
           min, max, length(args))
#define EXPECT_ARITY_MIN_1(args) EXPECT_ARITY_N(args != Qnil, ">= ", 1, 0)
#define EXPECT_ARITY_MIN_2(args) EXPECT_ARITY_N(is_length_min_2(args), ">= ", 2, length(args))

//
// Environments
//

[[gnu::nonnull(1)]]
static Value env_new(const char *name)
{
    Env *e = obj_new(TAG_ENV, sizeof(Env));
    e->table = NULL;
    e->parent = Qfalse;
    e->name = name;
    return (Value) e;
}

static Value env_dup(const char *name, const Value orig)
{
    if (ENV(orig)->parent != Qfalse)
        bug("duplication of chained environment not permitted");
    Env *e = obj_new(TAG_ENV, sizeof(Env));
    e->name = name != NULL ? name : ENV(orig)->name;
    const Table *t = ENV(orig)->table;
    e->table = t != NULL ? table_dup(t) : NULL;
    e->parent = Qfalse;
    return (Value) e;
}

static Value env_inherit(Value parent)
{
    Value e = env_new("tmp");
    ENV(e)->parent = parent;
    return e;
}

static void env_put(Value env, Value key, Value value)
{
    Table *t = ENV(env)->table;
    if (t == NULL)
        t = ENV(env)->table = table_new();
    table_put(t, SYMBOL(key), value);
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
[[nodiscard]] static Value eval(Value env, Value v);
#define eval_loc(env, loc) ({ \
            vValue R = eval((env), car(loc)); \
            EXPECT_ERROR_LOCATED(R, (loc)); \
            R; \
        })

static Value eval_body(Value env, Value body)
{
    Value last = Qfalse;
    for (Value p = body; p != Qnil; p = cdr(p))
        last = eval_loc(env, p);
    return last;
}

static Value map_eval(Value env, Value l)
{
    Value mapped = DUMMY_PAIR();
    for (Value last = mapped, p = l, v; p != Qnil; p = cdr(p)) {
        EXPECT_WITH_OBJ(sch_value_is_pair(p), "improper list for apply", l);
        v = eval_loc(env, p);
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

static bool frame_should_resolve(const StackFrame *f)
{
    return sch_value_is_integer(f->loc);
}

// Get LocatedPair of n-th argument in list
static Value resolve_location(StackFrame *const *f, Value list)
{
    int64_t n = sch_integer_to_cint(f[0]->loc);
    Value p = list; // begins with proc
    for (int64_t i = 0; i < n; i++)
        p = cdr(p);
    return p;
}

static Value push_stack_frame(Value ve, const char *name, Value loc)
{
    StackFrame **e = ERROR(ve);
    if (scary_length(e) > 0 && frame_should_resolve(e[0]))
        e[0]->loc = resolve_location(e, loc);
    StackFrame *f = stack_frame_new(name, loc);
    scary_push((void ***) &ERROR(ve), (const void *) f);
    return ve;
}

[[nodiscard]]
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

static Value expect_proper_list(Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p))
        EXPECT_WITH_OBJ(sch_value_is_pair(p), "improper list for apply", l);
    return l;
}

static Value eval_apply(Value env, Value l)
{
    Value proc = eval_loc(env, l), args = cdr(l), pargs;
    if (value_tag_is(proc, TAG_SYNTAX))
        pargs = expect_proper_list(args);
    else
        pargs = map_eval(env, args);
    EXPECT_ERROR_LOCATED(pargs, l);
    EXPECT_TYPE(procedure, proc);
    Value ret = apply(env, proc, pargs);
    if (is_error(ret)) {
        const char *fname = get_func_name(proc);
        return push_stack_frame(ret, fname, l);
    }
    return ret;
}

static Value lookup_or_error(Value env, Value v)
{
    Value p = env_get(env, v);
    EXPECT(p != Qundef, "unbound variable: %s", sch_symbol_to_cstr(v));
    return p;
}

static Value eval(Value env, Value v)
{
    if (sch_value_is_symbol(v))
        return lookup_or_error(env, v);
    if (!sch_value_is_pair(v))
        return v;
    return eval_apply(env, v);
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
    if (sch_value_is_symbol(sym))
        append_error_message("'%s'", sch_symbol_to_cstr(sym));
    else
        append_error_message("<unknown>");
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

static void add_source(const Source *newsrc)
{
    scary_push((void ***) &source_data, (const void *) newsrc);
    size_t len = scary_length(source_data);
    gc_add_root(&source_data[len-1]->ast);
}

Value sch_load_file(FILE *in, const char *filename)
{
    Source *src = iparse(in, filename);
    if (src == NULL)
        return Qundef;
    add_source(src);
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
    add_source(src);
    return eval_body(env_toplevel, src->ast);
}

Value sch_eval_string(const char *in)
{
    FILE *f = mopen(in);
    Value v = sch_load_file(f, "<inline>");
    fclose(f);
    return v;
}

#define scary_free_with(f, a) do { \
        for (size_t i = 0, len = scary_length(a); i < len; i++) \
            f(a[i]); \
        scary_free(a); \
    } while (0)

Value sch_eval_string_single(const char *in)
{
    scary_free_with(source_free, source_data);
    source_data = scary_new(sizeof(Source *));
    errmsg[0] = '\0';
    return sch_eval_string(in);
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
    if (in == NULL)
        error("can't open file: %s", path);
    Value retval = sch_load_file(in, path);
    fclose(in);
    return retval;
}

static Value load_inner(const char *path)
{
    const char *basedir_saved = load_basedir;
    FILE *in = open_loadable(path);
    EXPECT(in != NULL, "can't open file: %s", path);
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
        EXPECT_TYPE_OR(pair, symbol, params);
    EXPECT_TYPE(pair, body);
    return closure_new(env, params, body);
}

// 4.1.5. Conditionals
//PTR
static Value syn_if(Value env, Value args)
{
    EXPECT_ARITY_RANGE(2, 3, args);

    Value v = eval_loc(env, args), then = cdr(args), els = cddr(args);
    if (v != Qfalse)
        return eval_loc(env, then);
    if (els == Qnil)
        return Qfalse;
    return eval_loc(env, els);
}

// 4.1.6. Assignments
static Value iset(Value env, Value ident, Value val)
{
    bool found = env_set(env, ident, val);
    EXPECT(found, "unbound variable: %s", sch_symbol_to_cstr(ident));
    return Qfalse;
}

static Value syn_set(Value env, Value ident, Value expr)
{
    EXPECT_TYPE(symbol, ident);
    Value v = eval(env, expr);
    EXPECT_ERROR_LOCATED_BY_CALLER(v, 2);
    Value e = iset(env, ident, v);
    EXPECT_ERROR_LOCATED_BY_CALLER(e, 1);
    return Qfalse;
}

// 4.2. Derived expression types
// 4.2.1. Conditionals
static Value cond_eval_recipient(Value env, Value test, Value recipients)
{
    EXPECT_TYPE(pair, recipients);
    Value recipient = eval_loc(env, recipients);
    Value rest = cdr(recipients);
    EXPECT_TYPE(procedure, recipient);
    EXPECT(rest == Qnil, "only one expression expected after =>");
    return apply(env, recipient, list1(test));
}

//PTR
static Value syn_cond(Value env, Value clauses)
{
    EXPECT_ARITY_MIN_1(clauses);

    for (Value p = clauses; p != Qnil; p = cdr(p)) {
        Value clause = car(p);
        EXPECT_TYPE(pair, clause);
        Value test = car(clause);
        Value exprs = cdr(clause);
        if (test == SYM_ELSE)
            return eval_body(env, exprs);
        Value t = eval_loc(env, clause);
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

#define EXPECT_LIST_HEAD(v) \
    EXPECT(v == Qnil || sch_value_is_pair(v), \
           "expected null or pair but got %s", sch_value_to_type_name(v))
static Value memq(Value key, Value l);

//PTR
static Value syn_case(Value env, Value args)
{
    EXPECT_ARITY_MIN_2(args);
    Value key = eval_loc(env, args), clauses = cdr(args);
    EXPECT_LIST_HEAD(clauses);

    for (Value p = clauses; p != Qnil; p = cdr(p)) {
        Value clause = car(p);
        EXPECT_TYPE(pair, clause);
        Value data = car(clause), exprs = cdr(clause);
        EXPECT_TYPE(pair, exprs);
        if (data == SYM_ELSE)
            return eval_body(env, exprs);
        EXPECT_TYPE(pair, data);
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
        if ((last = eval_loc(env, p)) == Qfalse)
            break;
    }
    return last;
}

//PTR
static Value syn_or(Value env, Value args)
{
    Value last = Qfalse;
    for (Value p = args; p != Qnil; p = cdr(p)) {
        if ((last = eval_loc(env, p)) != Qfalse)
            break;
    }
    return last;
}

// 4.2.2. Binding constructs
static bool is_let_binding_form(Value b)
{
    return sch_value_is_pair(b) &&
        sch_value_is_symbol(car(b)) &&
        sch_value_is_pair(cdr(b)) &&
        cddr(b) == Qnil;
}

#define EXPECT_LET_BINDING_FORM(b) \
    EXPECT_WITH_OBJ(is_let_binding_form(b), "malformed binding", b)

static Value let(Value env, Value var, Value bindings, Value body)
{
    EXPECT_LIST_HEAD(bindings);
    bool named = var != Qfalse;
    Value localenv = env_inherit(env);
    Value params = DUMMY_PAIR(), lparams = params;
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        Value b = car(p);
        EXPECT_LET_BINDING_FORM(b);
        Value ident = car(b), exprs = cdr(b);
        if (named)
            lparams = PAIR(lparams)->cdr = list1(ident);
        Value val = eval_loc(env, exprs);
        env_put(localenv, ident, val);
    }
    if (named) {
        Value proc = closure_new(localenv, cdr(params), body);
        env_put(localenv, var, proc); // letenv affects as proc->env
    }
    return eval_body(localenv, body);
}

//PTR
static Value syn_let(Value env, Value args)
{
    EXPECT_ARITY_MIN_2(args);
    Value bind_or_var = car(args), body = cdr(args);
    Value var = Qfalse, bindings = bind_or_var;
    if (sch_value_is_symbol(bind_or_var)) {
        var = bind_or_var;
        bindings = car(body);
        body = cdr(body);
    }
    return let(env, var, bindings, body);
}

static Value let_star(Value env, Value bindings, Value body)
{
    EXPECT_LIST_HEAD(bindings);
    Value localenv = env;
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        vValue b = car(p); // workaround for clang -O2
        EXPECT_LET_BINDING_FORM(b);
        Value ident = car(b), exprs = cdr(b);
        localenv = env_inherit(localenv);
        Value val = eval_loc(localenv, exprs);
        env_put(localenv, ident, val);
    }
    return eval_body(localenv, body);
}

//PTR
static Value syn_let_star(Value env, Value args)
{
    EXPECT_ARITY_MIN_2(args);
    return let_star(env, car(args), cdr(args));
}

static Value letrec(Value env, Value bindings, Value body)
{
    EXPECT_LIST_HEAD(bindings);
    EXPECT_TYPE(pair, body);
    Value localenv = env_inherit(env);
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        Value b = car(p);
        EXPECT_LET_BINDING_FORM(b);
        Value ident = car(b), exprs = cdr(b);
        Value val = eval_loc(localenv, exprs);
        env_put(localenv, ident, val);
    }
    return eval_body(localenv, body);
}

//PTR
static Value syn_letrec(Value env, Value args)
{
    EXPECT_ARITY_MIN_2(args);
    return letrec(env, car(args), cdr(args));
}

// 4.2.3. Sequencing
//PTR
static Value syn_begin(Value env, Value body)
{
    return eval_body(env, body);
}

// 4.2.4. Iteration
static bool is_do_binding_form(Value b)
{
    return sch_value_is_pair(b) &&
        sch_value_is_symbol(car(b)) &&
        sch_value_is_pair(cdr(b)) &&
        (cddr(b) == Qnil || // length is 2
         (sch_value_is_pair(cddr(b)) && cdddr(b) == Qnil)); // or 3
}

#define EXPECT_DO_BINDING_FORM(b) EXPECT_WITH_OBJ(is_do_binding_form(b), "malformed binding", b)
#define EXPECT_UNIQUE_VARNAME(vars, v) \
    EXPECT(memq(v, vars) == Qfalse, "duplicated variable: %s", sch_symbol_to_cstr(v))

//PTR
static Value syn_do(Value env, Value args)
{
    EXPECT_ARITY_MIN_2(args);

    Value bindings = car(args), tests = cadr(args), body = cddr(args);
    EXPECT_LIST_HEAD(bindings);
    EXPECT_TYPE(pair, tests);

    Value localenv = env_inherit(env);
    Value steps = Qnil, vars = Qnil, v;
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        Value b = car(p);
        EXPECT_DO_BINDING_FORM(b);
        Value var = car(b), inits = cdr(b), step = cddr(b);
        EXPECT_UNIQUE_VARNAME(vars, var);
        vars = cons(var, vars);
        if (step != Qnil) {
            vValue datum = cons(var, step); // workaround for clang -Og
            steps = cons(datum, steps);
        }
        v = eval_loc(env, inits); // in the original env
        env_put(localenv, var, v);
    }
    Value exprs = cdr(tests);
    while ((v = eval_loc(localenv, tests)) == Qfalse) {
        if (body != Qnil) {
            v = eval_body(localenv, body);
            EXPECT_ERROR(v);
        }
        for (Value p = steps; p != Qnil; p = cdr(p)) {
            Value pstep = car(p);
            Value var = car(pstep), step = cdr(pstep);
            Value val = eval_loc(localenv, step);
            iset(localenv, var, val);
        }
    }
    return eval_body(localenv, exprs);
}

// 4.2.5. Delayed evaluation
static Value promise_new(Value env, Value val)
{
    Promise *pr = obj_new(TAG_PROMISE, sizeof(Promise));
    pr->forced = false;
    pr->env = env;
    pr->val = val;
    return (Value) pr;
}

static Value syn_delay(Value env, Value obj)
{
    return promise_new(env, obj);
}

// 4.2.6. Quasiquotation
static Value qq_list(Value env, Value datum, int64_t depth);

static Value qq(Value env, Value datum, int64_t depth)
{
    if (depth == 0)
        return eval(env, datum);
    if (datum == Qnil || !sch_value_is_pair(datum))
        return datum;
    Value a = car(datum), d = cdr(datum);
    if (a == SYM_QUASIQUOTE) {
        EXPECT_TYPE(pair, d);
        Value v = qq(env, car(d), depth + 1);
        EXPECT_ERROR(v);
        return list2_const(a, v);
    }
    if (a == SYM_UNQUOTE || a == SYM_UNQUOTE_SPLICING) {
        EXPECT_TYPE(pair, d);
        Value v = qq(env, car(d), depth - 1);
        EXPECT_ERROR(v);
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
    EXPECT_TYPE(pair, to_splice);
    if (last == Qnil)
        return to_splice;
    PAIR(last)->cdr = to_splice;
    return last_pair(to_splice);
}

static Value qq_list(Value env, Value datum, int64_t depth)
{
    Value ret = DUMMY_PAIR();
    for (Value last = ret, p = datum; p != Qnil; p = cdr(p)) {
        bool is_simple = !sch_value_is_pair(p);
        if (is_simple || is_quoted_terminal(p)) {
            EXPECT_TYPE(pair, cdr(ret));
            if (!is_simple) {
                p = qq(env, p, depth);
                EXPECT_ERROR(p);
            }
            PAIR(last)->cdr = p;
            break;
        }
        Value elem = car(p);
        bool spliced = (sch_value_is_pair(elem) && car(elem) == SYM_UNQUOTE_SPLICING);
        vValue v = qq(env, elem, depth); // workaround for clang -Og
        EXPECT_ERROR(v);
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
    EXPECT_TYPE(symbol, ident);

    Value val = eval(env, expr);
    EXPECT_ERROR(val);
    bool found = false;
    if (ENV(env)->parent == Qfalse)
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
    Value head = car(args), v;
    Type t = sch_value_type_of(head);
    switch (t) {
    case TYPE_SYMBOL:
        EXPECT_ARITY_2(args);
        v = define_variable(env, head, cadr(args));
        EXPECT_ERROR_LOCATED(v, cdr(args));
        return v;
    case TYPE_PAIR:
        v = define_proc_internal(env, head, cdr(args));
        EXPECT_ERROR_LOCATED(v, args);
        return v;
    case TYPE_NULL:
    case TYPE_BOOL:
    case TYPE_INT:
    case TYPE_CHAR:
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
    case TYPE_CHAR:
        return CHAR(x) == CHAR(y);
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

static Value proc_equal(UNUSED Value env, Value x, Value y)
{
    return BOOL_VAL(equal(x, y));
}

// 6.2. Numbers
// 6.2.5. Numerical operations
static Value proc_integer_p(UNUSED Value env, Value obj)
{
    return BOOL_VAL(sch_value_is_integer(obj));
}

#define get_int(x) ({ \
            Value X = x; \
            EXPECT_TYPE(integer, X); \
            INT(X); \
        })

typedef bool (*RelOpFunc)(int64_t x, int64_t y);
static Value relop(RelOpFunc func, Value args)
{
    EXPECT_ARITY_MIN_2(args);

    Value p = args;
    int64_t x = get_int(car(p));
    while ((p = cdr(p)) != Qnil) {
        int64_t y = get_int(car(p));
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
    return BOOL_VAL(sch_value_is_integer(obj) && INT(obj) == 0);
}

static Value proc_positive_p(UNUSED Value env, Value obj)
{
    return BOOL_VAL(sch_value_is_integer(obj) && INT(obj) > 0);
}

static Value proc_negative_p(UNUSED Value env, Value obj)
{
    return BOOL_VAL(sch_value_is_integer(obj) && INT(obj) < 0);
}

static Value proc_odd_p(UNUSED Value env, Value obj)
{
    return BOOL_VAL(sch_value_is_integer(obj) && INT(obj) % 2 != 0);
}

static Value proc_even_p(UNUSED Value env, Value obj)
{
    return BOOL_VAL(sch_value_is_integer(obj) && INT(obj) % 2 == 0);
}

static Value proc_max(UNUSED Value env, Value args)
{
    EXPECT_ARITY_MIN_1(args);
    int64_t max = get_int(car(args));
    for (Value p = cdr(args); p != Qnil; p = cdr(p)) {
        int64_t x = get_int(car(p));
        if (max < x)
            max = x;
    }
    return sch_integer_new(max);
}

static Value proc_min(UNUSED Value env, Value args)
{
    EXPECT_ARITY_MIN_1(args);
    int64_t min = get_int(car(args));
    for (Value p = cdr(args); p != Qnil; p = cdr(p)) {
        int64_t x = get_int(car(p));
        if (min > x)
            min = x;
    }
    return sch_integer_new(min);
}

static Value proc_add(UNUSED Value env, Value args)
{
    int64_t y = 0;
    for (Value p = args; p != Qnil; p = cdr(p))
        y += get_int(car(p));
    return sch_integer_new(y);
}

static Value proc_sub(UNUSED Value env, Value args)
{
    EXPECT_ARITY_MIN_1(args);

    int64_t y = get_int(car(args));
    Value p = cdr(args);
    if (p == Qnil)
        return sch_integer_new(-y);
    for (; p != Qnil; p = cdr(p))
        y -= get_int(car(p));
    return sch_integer_new(y);
}

static Value proc_mul(UNUSED Value env, Value args)
{
    int64_t y = 1;
    for (Value p = args; p != Qnil; p = cdr(p))
        y *= get_int(car(p));
    return sch_integer_new(y);
}

static inline Value divzero_error(void)
{
    return runtime_error("divided by zero");
}

static Value proc_div(UNUSED Value env, Value args)
{
    EXPECT_ARITY_MIN_1(args);

    int64_t y = get_int(car(args));
    Value p = cdr(args);
    if (p == Qnil) {
        if (y == 0)
            return divzero_error();
        return sch_integer_new(1 / y); // 0, 1, -1
    }
    for (; p != Qnil; p = cdr(p)) {
        int64_t x = get_int(car(p));
        if (x == 0)
            return divzero_error();
        y /= x;
    }
    return sch_integer_new(y);
}

static Value proc_abs(UNUSED Value env, Value x)
{
    int64_t n = get_int(x);
    return sch_integer_new(n < 0 ? -n : n);
}

static Value proc_quotient(UNUSED Value env, Value x, Value y)
{
    int64_t a = get_int(x), b = get_int(y);
    if (b == 0)
        return divzero_error();
    return sch_integer_new(a / b);
}

static Value proc_remainder(UNUSED Value env, Value x, Value y)
{
    int64_t a = get_int(x), b = get_int(y);
    if (b == 0)
        return divzero_error();
    int64_t c = a % b;
    return sch_integer_new(c);
}

static Value proc_modulo(UNUSED Value env, Value x, Value y)
{
    int64_t a = get_int(x), b = get_int(y);
    if (b == 0)
        return divzero_error();
    int64_t c = a % b;
    if ((a < 0 && b > 0) || (a > 0 && b < 0))
        c += b;
    return sch_integer_new(c);
}

static int64_t *list_to_int_abs_array(Value l, Value *err)
{
    int64_t *a = scary_new(sizeof(int64_t));
    for (Value p = l; p != Qnil; p = cdr(p)) {
        Value x = car(p);
        if (!sch_value_is_integer(x)) {
            *err = type_error("integer", x);
            return NULL;
        }
        int64_t i = INT(x);
        scary_push(&a, i < 0 ? -i : i);
    }
    return a;
}

static int64_t gcd(int64_t x, int64_t y)
{
    while (y != 0) {
        int64_t x2 = y;
        y = x % y;
        x = x2;
    }
    return x;
}

static Value proc_gcd(UNUSED Value env, Value args)
{
    if (args == Qnil)
        return sch_integer_new(0);
    Value err = Qfalse;
    int64_t *a = list_to_int_abs_array(args, &err);
    EXPECT_ERROR(err);
    int64_t ret = a[0];
    for (size_t i = 1, len = scary_length(a); i < len; i++)
        ret = gcd(ret, a[i]);
    scary_free(a);
    return sch_integer_new(ret);
}

static int64_t lcm(int64_t x, int64_t y)
{
    return llabs(x * y) / gcd(x, y);
}

static Value proc_lcm(UNUSED Value env, Value args)
{
    if (args == Qnil)
        return sch_integer_new(1);
    Value err = Qfalse;
    int64_t *a = list_to_int_abs_array(args, &err);
    EXPECT_ERROR(err);
    int64_t ret = a[0];
    for (size_t i = 1, len = scary_length(a); i < len; i++)
        ret = lcm(ret, a[i]);
    scary_free(a);
    return sch_integer_new(ret);
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

#define get_non_negative_int(x) ({ \
            int64_t N = get_int(x); \
            EXPECT(N >= 0, "must be non-negative: %"PRId64, N); \
            N; \
        })

static Value proc_expt(UNUSED Value env, Value x, Value y)
{
    int64_t a = get_int(x), b = get_non_negative_int(y);
    int64_t c;
    if (b == 0)
        c = 1;
    else if (a == 0)
        c = 0;
    else
        c = expt(a, b);
    return sch_integer_new(c);
}

// 6.2.6. Numerical input and output
static Value proc_number_to_string(UNUSED Value env, Value args)
{
    EXPECT_ARITY_RANGE(1, 2, args);
    int64_t n = get_int(car(args));
    int64_t radix = (cdr(args) != Qnil) ? get_int(cadr(args)) : 10;
    char buf[66]; // in case of radix == 2 (64-bit integer) + sign + \0
    if (radix == 10) {
        snprintf(buf, sizeof(buf), "%"PRId64, n);
        return sch_string_new(buf);
    }
    const char *sign = "";
    if (n < 0) {
        sign = "-";
        n *= -1;
    }
    switch (radix) {
    case 2:
        snprintf(buf, sizeof(buf), "%s%"PRIb64, sign, n);
        break;
    case 8:
        snprintf(buf, sizeof(buf), "%s%"PRIo64, sign, n);
        break;
    case 16:
        snprintf(buf, sizeof(buf), "%s%"PRIX64, sign, n);
        break;
    default:
        return runtime_error("invalid radix: %"PRId64, radix);
    }
    return sch_string_new(buf);
}

static Value proc_string_to_number(UNUSED Value env, Value args)
{
    EXPECT_ARITY_RANGE(1, 2, args);
    Value s = car(args);
    EXPECT_TYPE(string, s);
    if (STRING(s)[0] == '\0')
        return Qfalse;
    int64_t radix = (cdr(args) != Qnil) ? get_int(cadr(args)) : 10;
    char *ep;
    errno = 0;
    int64_t i = strtoll(STRING(s), &ep, radix);
    if (errno != 0 || (i == 0 && STRING(s) == ep))
        return Qfalse;
    return sch_integer_new(i);
}

// 6.3. Other data types
// 6.3.1. Booleans
static Value proc_not(UNUSED Value env, Value x)
{
    return BOOL_VAL(x == Qfalse);
}

static Value proc_boolean_p(UNUSED Value env, Value x)
{
    return BOOL_VAL(is_boolean(x));
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

static Value cons_const(Value car, Value cdr)
{
    Value v = cons(car, cdr);
    HEADER(v)->immutable = true;
    return v;
}

inline Value list1(Value x)
{
    return cons(x, Qnil);
}

inline Value list1_const(Value x)
{
    return cons_const(x, Qnil);
}

inline Value list2_const(Value x, Value y)
{
    return cons_const(x, list1_const(y));
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
    EXPECT_TYPE(pair, pair);
    return car(pair);
}

static Value proc_cdr(UNUSED Value env, Value pair)
{
    EXPECT_TYPE(pair, pair);
    return cdr(pair);
}

#define EXPECT_MUTABLE(o) \
    EXPECT(!HEADER(o)->immutable, "cannot modify immutable object")

static Value proc_set_car(UNUSED Value env, Value pair, Value obj)
{
    EXPECT_TYPE(pair, pair);
    EXPECT_MUTABLE(pair);
    PAIR(pair)->car = obj;
    return pair;
}

static Value proc_set_cdr(UNUSED Value env, Value pair, Value obj)
{
    EXPECT_TYPE(pair, pair);
    EXPECT_MUTABLE(pair);
    PAIR(pair)->cdr = obj;
    return pair;
}

static Value safe_car(Value v)
{
    EXPECT_ERROR(v);
    EXPECT_TYPE(pair, v);
    return car(v);
}

static Value safe_cdr(Value v)
{
    EXPECT_ERROR(v);
    EXPECT_TYPE(pair, v);
    return cdr(v);
}

#define DEF_SAFE_CXR(x, y) \
    static Value safe_c##x##y##r(Value v) { return safe_c##x##r(safe_c##y##r(v)); }
CXRS(DEF_SAFE_CXR)

#define DEF_CXR_BUILTIN(x, y) \
    static Value proc_c##x##y##r(UNUSED Value env, Value v) \
    { \
        return safe_c##x##y##r(v); \
    }
CXRS(DEF_CXR_BUILTIN)

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
    EXPECT_LIST_HEAD(list);
    return sch_integer_new(length(list));
}

static Value dup_list(Value l, Value *plast)
{
    if (l == Qnil)
        return Qnil;
    Value dup = DUMMY_PAIR(), last = dup;
    for (Value p = l; p != Qnil; p = cdr(p)) {
        EXPECT_TYPE(pair, p);
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
        EXPECT_LIST_HEAD(pl);
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

static Value reverse(Value l)
{
    Value ret = Qnil;
    for (Value p = l; p != Qnil; p = cdr(p))
        ret = cons(car(p), ret);
    return ret;
}

static Value proc_reverse(UNUSED Value env, Value list)
{
    EXPECT_LIST_HEAD(list);
    return reverse(list);
}

static Value list_tail(Value list, int64_t k)
{
    EXPECT_LIST_HEAD(list);
    Value p = list;
    int64_t i;
    for (i = 0; p != Qnil; p = cdr(p), i++) {
        if (i == k)
            break;
    }
    EXPECT(i == k, "list is shorter than %"PRId64, k);
    return p;
}

static Value proc_list_tail(UNUSED Value env, Value list, Value vk)
{
    int64_t k = get_non_negative_int(vk);
    return list_tail(list, k);
}

static Value proc_list_ref(UNUSED Value env, Value list, Value vk)
{
    int64_t k = get_non_negative_int(vk);
    Value tail = list_tail(list, k);
    EXPECT(tail != Qnil, "list is not longer than %"PRId64, INT(k));
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
    EXPECT_LIST_HEAD(list);
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
    EXPECT_LIST_HEAD(list);
    return member(obj, list);
}

static Value assq(Value key, Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        Value e = car(p);
        if (sch_value_is_pair(e) && car(e) == key)
            return e;
    }
    return Qfalse;
}

static Value proc_assq(UNUSED Value env, Value obj, Value alist)
{
    EXPECT_LIST_HEAD(alist);
    return assq(obj, alist);
}

static Value assoc(Value key, Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        Value e = car(p);
        if (sch_value_is_pair(e) && equal(car(e), key))
            return e;
    }
    return Qfalse;
}

static Value proc_assoc(UNUSED Value env, Value obj, Value alist)
{
    EXPECT_LIST_HEAD(alist);
    return assoc(obj, alist);
}

// 6.3.3. Symbols
static Value proc_symbol_p(UNUSED Value env, Value obj)
{
    return BOOL_VAL(sch_value_is_symbol(obj));
}

Value sch_string_immutable_new(const char *str)
{
    Value s = sch_string_new(str);
    HEADER(s)->immutable = true;
    return s;
}

static Value proc_symbol_to_string(UNUSED Value env, Value sym)
{
    EXPECT_TYPE(symbol, sym);
    const char *s = sch_symbol_to_cstr(sym);
    return sch_string_immutable_new(s);
}

static Value proc_string_to_symbol(UNUSED Value env, Value str)
{
    EXPECT_TYPE(string, str);
    return sch_symbol_new(STRING(str));
}

// 6.3.4. Characters
static Value proc_char_p(UNUSED Value env, Value ch)
{
    return BOOL_VAL(sch_value_is_character(ch));
}

#define CHAR_CMP(c1, c2, op) BOOL_VAL(CHAR(c1) op CHAR(c2))
#define CHAR_CI_CMP(c1, c2, op) BOOL_VAL(tolower(CHAR(c1)) op tolower(CHAR(c2)))

static Value proc_char_eq_p(UNUSED Value env, Value c1, Value c2)
{
    EXPECT_TYPE_TWIN(character, c1, c2);
    return CHAR_CMP(c1, c2, ==);
}

static Value proc_char_lt_p(UNUSED Value env, Value c1, Value c2)
{
    EXPECT_TYPE_TWIN(character, c1, c2);
    return CHAR_CMP(c1, c2, <);
}

static Value proc_char_gt_p(UNUSED Value env, Value c1, Value c2)
{
    EXPECT_TYPE_TWIN(character, c1, c2);
    return CHAR_CMP(c1, c2, >);
}

static Value proc_char_le_p(UNUSED Value env, Value c1, Value c2)
{
    EXPECT_TYPE_TWIN(character, c1, c2);
    return CHAR_CMP(c1, c2, <=);
}

static Value proc_char_ge_p(UNUSED Value env, Value c1, Value c2)
{
    EXPECT_TYPE_TWIN(character, c1, c2);
    return CHAR_CMP(c1, c2, >=);
}

static Value proc_char_ci_eq_p(UNUSED Value env, Value c1, Value c2)
{
    EXPECT_TYPE_TWIN(character, c1, c2);
    return CHAR_CI_CMP(c1, c2, ==);
}

static Value proc_char_ci_lt_p(UNUSED Value env, Value c1, Value c2)
{
    EXPECT_TYPE_TWIN(character, c1, c2);
    return CHAR_CI_CMP(c1, c2, <);
}

static Value proc_char_ci_gt_p(UNUSED Value env, Value c1, Value c2)
{
    EXPECT_TYPE_TWIN(character, c1, c2);
    return CHAR_CI_CMP(c1, c2, >);
}

static Value proc_char_ci_le_p(UNUSED Value env, Value c1, Value c2)
{
    EXPECT_TYPE_TWIN(character, c1, c2);
    return CHAR_CI_CMP(c1, c2, <=);
}

static Value proc_char_ci_ge_p(UNUSED Value env, Value c1, Value c2)
{
    EXPECT_TYPE_TWIN(character, c1, c2);
    return CHAR_CI_CMP(c1, c2, >=);
}

static Value proc_char_alphabetic_p(UNUSED Value env, Value c)
{
    EXPECT_TYPE(character, c);
    return BOOL_VAL(isalpha(CHAR(c)));
}

static Value proc_char_numeric_p(UNUSED Value env, Value c)
{
    EXPECT_TYPE(character, c);
    return BOOL_VAL(isdigit(CHAR(c)));
}

static Value proc_char_whitespace_p(UNUSED Value env, Value c)
{
    EXPECT_TYPE(character, c);
    return BOOL_VAL(isspace(CHAR(c)));
}

static Value proc_char_upper_case_p(UNUSED Value env, Value c)
{
    EXPECT_TYPE(character, c);
    return BOOL_VAL(isupper(CHAR(c)));
}

static Value proc_char_to_integer(UNUSED Value env, Value c)
{
    EXPECT_TYPE(character, c);
    return sch_integer_new(CHAR(c));
}

static Value proc_integer_to_char(UNUSED Value env, Value c)
{
    int64_t i = get_int(c);
    if (i < 0 || i > 0xFF)
        return runtime_error("integer out of domain");
    return sch_character_new(i);
}

static Value proc_char_lower_case_p(UNUSED Value env, Value c)
{
    EXPECT_TYPE(character, c);
    return BOOL_VAL(islower(CHAR(c)));
}

static Value proc_char_upcase(UNUSED Value env, Value c)
{
    EXPECT_TYPE(character, c);
    return sch_character_new(toupper(CHAR(c)));
}

static Value proc_char_downcase(UNUSED Value env, Value c)
{
    EXPECT_TYPE(character, c);
    return sch_character_new(tolower(CHAR(c)));
}

// 6.3.5. Strings
static Value proc_string_p(UNUSED Value env, Value obj)
{
    return BOOL_VAL(sch_value_is_string(obj));
}

#define EXPECT_C_CHAR(v) do { \
        EXPECT_TYPE(character, v); \
        EXPECT(CHAR(v) != '\0', "cannot use '\\0' as a character in strings currently"); \
    } while (0)

static Value proc_make_string(UNUSED Value env, Value args)
{
    EXPECT_ARITY_RANGE(1, 2, args);
    int k = get_non_negative_int(car(args));
    uint8_t pad = ' ';
    if (cdr(args) != Qnil) {
        Value ch = cadr(args);
        EXPECT_C_CHAR(ch);
        pad = CHAR(ch);
    }
    char s[k + 1];
    memset(s, pad, k);
    s[k] = '\0';
    return sch_string_new(s);
}

static Value proc_string(UNUSED Value env, Value args)
{
    int64_t len = length(args), i = 0;
    char s[len + 1];
    for (Value p = args; p != Qnil; p = cdr(p), i++) {
        Value ch = car(p);
        EXPECT_C_CHAR(ch);
        s[i] = CHAR(ch);
    }
    s[len] = '\0';
    return sch_string_new(s);
}

static Value proc_string_length(UNUSED Value env, Value s)
{
    EXPECT_TYPE(string, s);
    return sch_integer_new(strlen(STRING(s)));
}

static Value expect_string_with_valid_index(Value vs, Value vk)
{
    EXPECT_TYPE(string, vs);
    int64_t k = get_non_negative_int(vk);
    const char *s = STRING(vs);
    ssize_t len = strlen(s);
    EXPECT(k < len, "invalid index %"PRId64, k);
    return Qfalse;
}

#define EXPECT_STRING_WITH_VALID_INDEX(s, k) EXPECT_ERROR(expect_string_with_valid_index(s, k))

static Value proc_string_ref(UNUSED Value env, Value vs, Value vk)
{
    EXPECT_STRING_WITH_VALID_INDEX(vs, vk);
    const char *s = STRING(vs);
    int64_t k = INT(vk);
    return sch_character_new(s[k]);
}

static Value proc_string_set(UNUSED Value env, Value vs, Value vk, Value ch)
{
    EXPECT_MUTABLE(vs);
    EXPECT_C_CHAR(ch);
    EXPECT_STRING_WITH_VALID_INDEX(vs, vk);
    char *s = STRING(vs);
    int64_t k = INT(vk);
    s[k] = CHAR(ch);
    return Qfalse;
}

#define STRING_CMP(c1, c2, op) BOOL_VAL(strcmp(STRING(c1), STRING(c2)) op 0)
#define STRING_CI_CMP(c1, c2, op) BOOL_VAL(strcasecmp(STRING(c1), STRING(c2)) op 0)

static Value proc_string_eq(UNUSED Value env, Value s1, Value s2)
{
    EXPECT_TYPE_TWIN(string, s1, s2);
    return STRING_CMP(s1, s2, ==);
}

static Value proc_string_ci_eq(UNUSED Value env, Value s1, Value s2)
{
    EXPECT_TYPE_TWIN(string, s1, s2);
    return STRING_CI_CMP(s1, s2, ==);
}

static Value proc_string_lt(UNUSED Value env, Value s1, Value s2)
{
    EXPECT_TYPE_TWIN(string, s1, s2);
    return STRING_CMP(s1, s2, <);
}

static Value proc_string_gt(UNUSED Value env, Value s1, Value s2)
{
    EXPECT_TYPE_TWIN(string, s1, s2);
    return STRING_CMP(s1, s2, >);
}

static Value proc_string_le(UNUSED Value env, Value s1, Value s2)
{
    EXPECT_TYPE_TWIN(string, s1, s2);
    return STRING_CMP(s1, s2, <=);
}

static Value proc_string_ge(UNUSED Value env, Value s1, Value s2)
{
    EXPECT_TYPE_TWIN(string, s1, s2);
    return STRING_CMP(s1, s2, >=);
}

static Value proc_string_ci_lt(UNUSED Value env, Value s1, Value s2)
{
    EXPECT_TYPE_TWIN(string, s1, s2);
    return STRING_CI_CMP(s1, s2, <);
}

static Value proc_string_ci_gt(UNUSED Value env, Value s1, Value s2)
{
    EXPECT_TYPE_TWIN(string, s1, s2);
    return STRING_CI_CMP(s1, s2, >);
}

static Value proc_string_ci_le(UNUSED Value env, Value s1, Value s2)
{
    EXPECT_TYPE_TWIN(string, s1, s2);
    return STRING_CI_CMP(s1, s2, <=);
}

static Value proc_string_ci_ge(UNUSED Value env, Value s1, Value s2)
{
    EXPECT_TYPE_TWIN(string, s1, s2);
    return STRING_CI_CMP(s1, s2, >=);
}

static Value proc_substring(UNUSED Value env, Value string, Value vstart, Value vend)
{
    EXPECT_TYPE(string, string);
    const char *s = STRING(string);
    int64_t start = get_non_negative_int(vstart), end = get_non_negative_int(vend);
    EXPECT(start <= end, "start index %"PRId64" must be <= end index %"PRId64, start, end);
    size_t len = strlen(s);
    EXPECT((size_t) end <= len, "end index %"PRId64" must be <= string length %zu", end, len);
    size_t newlen = end - start;
    char buf[newlen+1];
    strncpy(buf, s + start, newlen);
    buf[newlen] = '\0';
    return sch_string_new(buf);
}

static Value proc_string_append(UNUSED Value env, Value args)
{
    EXPECT_LIST_HEAD(args);
    size_t len = 0;
    for (Value p = args, v; p != Qnil; p = cdr(p)) {
        v = car(p);
        EXPECT_TYPE(string, v);
        len += strlen(STRING(v));
    }
    char *s = xmalloc(len + 1);
    s[0] = '\0';
    for (Value p = args; p != Qnil; p = cdr(p))
        strcat(s, STRING(car(p)));
    return string_new_moved(s);
}

static Value proc_string_to_list(UNUSED Value env, Value str)
{
    EXPECT_TYPE(string, str);
    const char *s = STRING(str);
    Value l = DUMMY_PAIR(), last = l;
    for (size_t i = 0, len = strlen(s); i < len; i++)
        last = PAIR(last)->cdr = list1(sch_character_new(s[i]));
    return cdr(l);
}

static Value proc_list_to_string(UNUSED Value env, Value l)
{
    EXPECT_LIST_HEAD(l);
    char *s = scary_new(sizeof(char));
    for (Value p = l; p != Qnil; p = cdr(p)) {
        Value e = car(p);
        EXPECT_C_CHAR(e);
        scary_push(&s, (char) CHAR(e));
    }
    scary_push(&s, (char) '\0');
    Value ret = sch_string_new((s));
    scary_free(s);
    return ret;
}

static Value proc_string_copy(UNUSED Value env, Value str)
{
    EXPECT_TYPE(string, str);
    return sch_string_new(STRING(str));
}

static Value proc_string_fill(UNUSED Value env, Value str, Value ch)
{
    EXPECT_TYPE(string, str);
    EXPECT_C_CHAR(ch);
    char *p = STRING(str);
    size_t len = strlen(p);
    memset(p, CHAR(ch), len);
    return Qfalse;
}

// 6.3.6. Vectors
Value vector_new(void)
{
    Vector *v = obj_new(TAG_VECTOR, sizeof(Vector));
    VECTOR(v) = scary_new(sizeof(Value));
    return (Value) v;
}

Value vector_push(Value v, Value e)
{
    scary_push(&VECTOR(v), e);
    return v;
}

static Value proc_vector_p(UNUSED Value env, Value o)
{
    return BOOL_VAL(value_tag_is(o, TAG_VECTOR));
}

static Value list_to_vector(Value l);

static Value proc_vector(UNUSED Value env, Value args)
{
    return list_to_vector(args);
}

static Value proc_make_vector(UNUSED Value env, Value args)
{
    EXPECT_ARITY_RANGE(1, 2, args);
    size_t k = get_non_negative_int(car(args));
    Value fill = Qfalse;
    if (cdr(args) != Qnil)
        fill = cadr(args);
    Value v = vector_new();
    for (size_t i = 0; i < k; i++)
        vector_push(v, fill);
    return v;
}

static Value proc_vector_length(UNUSED Value env, Value v)
{
    EXPECT_TYPE(vector, v);
    return sch_integer_new(scary_length(VECTOR(v)));
}

static Value proc_vector_ref(UNUSED Value env, Value o, Value k)
{
    EXPECT_TYPE(vector, o);
    size_t i = get_non_negative_int(k);
    Value *v = VECTOR(o);
    if (i >= scary_length(v))
        return Qfalse;
    return v[i];
}

static Value proc_vector_set(UNUSED Value env, Value o, Value k, Value obj)
{
    EXPECT_TYPE(vector, o);
    EXPECT_MUTABLE(o);
    size_t i = get_non_negative_int(k);
    Value *v = VECTOR(o);
    if (i < scary_length(v))
        v[i] = obj;
    return Qfalse;
}

static Value vector_to_list(Value *v)
{
    Value l = Qnil;
    for (int64_t i = scary_length(v) - 1; i >= 0; i--)
        l = cons(v[i], l) ;
    return l;
}

static Value list_to_vector(Value l)
{
    Value v = vector_new();
    for (Value p = l; p != Qnil; p = cdr(p))
        vector_push(v, car(p));
    return v;
}

static Value proc_vector_to_list(UNUSED Value env, Value v)
{
    EXPECT_TYPE(vector, v);
    return vector_to_list(VECTOR(v));
}

static Value proc_list_to_vector(UNUSED Value env, Value l)
{
    EXPECT_LIST_HEAD(l);
    return list_to_vector(l);
}

static Value proc_vector_fill(UNUSED Value env, Value vec, Value fill)
{
    EXPECT_TYPE(vector, vec);
    Value *v = VECTOR(vec);
    for (size_t i = 0, len = scary_length(v); i < len; i++)
        v[i] = fill;
    return vec;
}

// 6.4. Control features
static Value proc_procedure_p(UNUSED Value env, Value o)
{
    return BOOL_VAL(sch_value_is_procedure(o));
}

static Value build_apply_args(Value args)
{
    Value heads = DUMMY_PAIR(), p = args, last = heads;
    for (Value next; (next = cdr(p)) != Qnil; p = next)
        last = PAIR(last)->cdr = list1(car(p));
    Value rest = car(p);
    EXPECT_LIST_HEAD(rest);
    PAIR(last)->cdr = rest;
    return cdr(heads);
}

static Value proc_apply(Value env, Value args)
{
    EXPECT_ARITY_MIN_2(args);

    Value proc = car(args);
    EXPECT_TYPE(procedure, proc);
    Value appargs = build_apply_args(cdr(args));
    EXPECT_ERROR(appargs);
    return apply(env, proc, appargs);
}

#define EXPECT_LIST_OF_LISTS(ls) do { \
        for (Value p = ls; p != Qnil; p = cdr(p)) \
            EXPECT_LIST_HEAD(car(p)); \
    } while (0)
#define EXPECT_LIST_OF_NIL(ls)  do { \
        for (Value p = ls; p != Qnil; p = cdr(p)) \
            EXPECT(car(p) == Qnil, "different length list in arguments"); \
    } while (0)

static Value get_cars(Value *pls)
{
    Value ls = *pls;
    if (ls == Qnil)
        return Qnil;
    Value first = car(ls);
    if (first == Qnil) {
        EXPECT_LIST_OF_NIL(cdr(ls));
        return Qnil;
    }
    EXPECT_LIST_OF_LISTS(ls);
    Value cars = list1(car(first)), cdrs = list1(cdr(first));
    for (Value p = cdr(ls), lcars = cars, lcdrs = cdrs; p != Qnil; p = cdr(p)) {
        Value l = car(p);
        EXPECT(l != Qnil, "different length list in arguments");
        lcars = PAIR(lcars)->cdr = list1(car(l));
        lcdrs = PAIR(lcdrs)->cdr = list1(cdr(l));
    }
    *pls = cdrs;
    return cars;
}

static Value proc_map(Value env, Value args)
{
    EXPECT_ARITY_MIN_2(args);

    Value proc = car(args);
    EXPECT_TYPE(procedure, proc);
    Value ls = cdr(args), ret = DUMMY_PAIR();
    for (Value last = ret, cars, v; (cars = get_cars(&ls)) != Qnil; ) {
        EXPECT_ERROR(cars);
        v = apply(env, proc, cars);
        EXPECT_ERROR(v);
        last = PAIR(last)->cdr = list1(v);
    }
    return cdr(ret);
}

static Value proc_for_each(Value env, Value args)
{
    EXPECT_ARITY_MIN_2(args);

    Value proc = car(args);
    EXPECT_TYPE(procedure, proc);
    for (Value ls = cdr(args), cars, v; (cars = get_cars(&ls)) != Qnil; ) {
        EXPECT_ERROR(cars);
        v = apply(env, proc, cars);
        EXPECT_ERROR(v);
    }
    return Qfalse;
}

static Value proc_force(UNUSED Value env, Value obj)
{
    if (!sch_value_is_promise(obj))
        return obj;
    Promise *pr = PROMISE(obj);
    if (!pr->forced) {
        Value val = eval(pr->env, pr->val);
        EXPECT_ERROR(val);
        pr->env = Qfalse; // env not needed anymore
        pr->val = val;
        pr->forced = true;
    }
    return pr->val;
}

[[noreturn, gnu::noinline]]
static void jump(Continuation *cont)
{
    memcpy(cont->sp, cont->stack, cont->stack_len);
    longjmp(cont->state, 1);
}

[[gnu::noinline]]
static Value apply_continuation(UNUSED Value env, Value f, Value args)
{
    GET_SP(sp);
    EXPECT_ARITY(PROCEDURE(f)->arity, args);
    Continuation *cont = CONTINUATION(f);
    cont->retval = PROCEDURE(f)->arity == 1 ? car(args) : args;
    size_t d = iceil(ptrdiff_abs(sp, cont->sp), sizeof(uintptr_t));
    if (d < 1)
        d = 1;
    volatile uintptr_t pad[d];
    pad[0] = pad[d-1] = 0; // avoid unused
    jump(cont);
}

static Value continuation_new(int64_t n)
{
    Continuation *c = obj_new(TAG_CONTINUATION, sizeof(Continuation));
    c->proc.arity = n; // call/cc: 1, call-with-values: -1
    c->proc.apply = apply_continuation;
    c->retval = Qfalse;
    c->sp = NULL;
    c->stack = NULL;
    c->stack_len = 0;
    return (Value) c;
}

[[gnu::noinline, gnu::no_sanitize_address]]
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

// shared with dynamic-wind
static Value callcc(Value env, Value proc)
{
    Value c = continuation_new(1);
    if (continuation_set(c))
        return CONTINUATION(c)->retval;
    return apply(env, proc, list1(c));
}

static Value do_wind(Value new_winders);

static Value callcc_inner2(Value pair, Value arg)
{
    Value k = car(pair), saved = cdr(pair);
    if (saved != inner_winders)
        EXPECT_ERROR(do_wind(saved));
    return apply(Qfalse, k, list1(arg));
}

static Value callcc_inner1(Value proc, Value k)
{
    Value saved = inner_winders, pair = cons(k, saved);
    Value arg = cfunc_closure_new(".callcc-inner2", callcc_inner2, pair);
    return apply(Qfalse, proc, list1(arg));
}

static Value proc_callcc(Value env, Value proc)
{
    EXPECT_TYPE(procedure, proc);
    Value inner = cfunc_closure_new(".callcc-inner1", callcc_inner1, proc);
    return callcc(env, inner);
}

static Value proc_values(Value env, Value args)
{
    if (inner_continuation == Qfalse)
        return car(args);
    return apply(env, inner_continuation, args);
}

static Value proc_call_with_values(Value env, Value producer, Value consumer)
{
    EXPECT_TYPE_TWIN(procedure, producer, consumer);

    Value k = continuation_new(-1), origk = inner_continuation;
    Value args;
    if (continuation_set(k))
        args = CONTINUATION(k)->retval;
    else {
        inner_continuation = k;
        Value val = apply(env, producer, Qnil);
        EXPECT_ERROR(val);
        args = list1(val);
    }
    inner_continuation = origk;
    return apply(env, consumer, args);
}

static Value common_tail(Value x, Value y)
{
    int64_t lx = length(x), ly = length(y);
    if (lx > ly)
        x = list_tail(x, lx - ly);
    else if (ly > lx)
        y = list_tail(y, ly - lx);
    Value px = x;
    for (Value py = y;  px != py; px = cdr(px), py = cdr(py))
        ;
    return px;
}

static Value do_wind(Value new_winders)
{
    Value tail = common_tail(new_winders, inner_winders);
    for (Value ls = inner_winders; ls != tail; ls = cdr(ls)) {
        inner_winders = cdr(ls);
        Value f = cdar(ls);
        EXPECT_ERROR(apply(Qfalse, f, Qnil));
    }
    Value rev = Qnil;
    for (Value p = new_winders; p != tail; p = cdr(p))
        rev = cons(car(p), rev);
    for (Value p = rev; p != Qnil; p = cdr(p)) {
        Value f = caar(p);
        EXPECT_ERROR(apply(Qfalse, f, Qnil));
        inner_winders = p;
    }
    return Qfalse;
}

static Value proc_dynamic_wind(Value env, Value before, Value thunk, Value after)
{
    EXPECT_TYPE(procedure, before);
    EXPECT_TYPE(procedure, thunk);
    EXPECT_TYPE(procedure, after);

    Value err = apply(env, before, Qnil);
    EXPECT_ERROR(err);
    inner_winders = cons(cons(before, after), inner_winders);
    Value ret = apply(env, thunk, Qnil);
    EXPECT_ERROR(ret);
    inner_winders = cdr(inner_winders);
    err = apply(env, after, Qnil);
    EXPECT_ERROR(err);
    return ret;
}

// 6.5. Eval
static Value proc_eval(UNUSED Value genv, Value expr, Value env)
{
    EXPECT_TYPE(env, env);
    return eval(env, expr);
}

#define EXPECT_R5RS(ver) \
    EXPECT(sch_value_is_integer(version) && INT(version) == 5, \
           "only integer '5' is allowed for environment version")

static Value proc_scheme_report_environment(UNUSED Value env, Value version)
{
    EXPECT_R5RS(version);
    return env_dup(NULL, env_r5rs);
}

static Value proc_null_environment(UNUSED Value env, Value version)
{
    EXPECT_R5RS(version);
    return env_dup(NULL, env_null);
}

static Value proc_interaction_environment(UNUSED Value env)
{
    return env_dup("interaction", env_default); // alias
}

// 6.6. Input and output
// 6.6.1. Ports
static Value open_port(const char *path, PortType type);
static void close_port(Value port);

static Value call_with_file(Value env, Value path, Value thunk, PortType type)
{
    Value file = open_port(STRING(path), type);
    EXPECT_ERROR(file);
    Value ret = apply(env, thunk, list1(file));
    close_port(file);
    return ret;
}

static Value proc_call_with_input_file(Value env, Value path, Value thunk)
{
    EXPECT_TYPE(string, path);
    return call_with_file(env, path, thunk, PORT_INPUT);
}

static Value proc_call_with_output_file(Value env, Value path, Value thunk)
{
    EXPECT_TYPE(string, path);
    return call_with_file(env, path, thunk, PORT_OUTPUT);
}

static Value proc_input_port_p(UNUSED Value env, Value port)
{
    return BOOL_VAL(value_tag_is(port, TAG_PORT) && PORT(port)->type == PORT_INPUT);
}

static Value proc_output_port_p(UNUSED Value env, Value port)
{
    return BOOL_VAL(value_tag_is(port, TAG_PORT) && PORT(port)->type == PORT_OUTPUT);
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

static Value proc_current_input_port(UNUSED Value env)
{
    return get_current_input_port();
}

static Value proc_current_output_port(UNUSED Value env)
{
    return get_current_output_port();
}

static Value open_port(const char *path, PortType type)
{
    bool output = type == PORT_OUTPUT;
    const char *mode = output ? "w" : "r";
    FILE *fp = fopen(path, mode);
    EXPECT(fp != NULL,
           "cannot open %s file: %s", output ? "output" : "input", path);
    return port_new(fp, type);
}

static Value proc_open_input_file(UNUSED Value env, Value path)
{
    EXPECT_TYPE(string, path);
    return open_port(STRING(path), PORT_INPUT);
}

static Value proc_open_output_file(UNUSED Value env, Value path)
{
    EXPECT_TYPE(string, path);
    return open_port(STRING(path), PORT_OUTPUT);
}

static Value with_file(Value env, Value path, Value thunk, Value *curr_port, PortType type)
{
    Value orig = *curr_port;
    Value newer = open_port(STRING(path), type);
    EXPECT_ERROR(newer);
    *curr_port = newer;
    Value ret = apply(env, thunk, Qnil);
    close_port(*curr_port);
    *curr_port = orig;
    return ret;
}

static Value proc_with_input_from_file(Value env, Value path, Value thunk)
{
    EXPECT_TYPE(string, path);
    EXPECT_TYPE(procedure, thunk);
    return with_file(env, path, thunk, &current_input_port, PORT_INPUT);
}

static Value proc_with_output_to_file(Value env, Value path, Value thunk)
{
    EXPECT_TYPE(string, path);
    EXPECT_TYPE(procedure, thunk);
    return with_file(env, path, thunk, &current_output_port, PORT_OUTPUT);
}

static void close_port(Value v)
{
    Port *p = PORT(v);
    if (p->fp == NULL)
        return;
    fclose(p->fp);
    p->fp = NULL; // guarantee safety
    if (p->string != (void *) 1U) // avoid to free dummy in string port
        free(p->string);
    p->string = NULL;
}

static Value proc_close_port(UNUSED Value env, Value port)
{
    EXPECT_TYPE(port, port);
    close_port(port);
    return Qfalse;
}

static const char *port_type_string(PortType type)
{
    return type == PORT_INPUT ? "input" : "output";
}

static inline Value invalid_port_type_error(PortType expected, Value port)
{
    return runtime_error("expected %s port but got %s port",
                         port_type_string(expected),
                         port_type_string(PORT(port)->type));
}

#define EXPECT_PORT(exptype, port) \
    EXPECT_OR_RETURN(sch_value_is_port(port) && PORT(port)->type == exptype, \
                     invalid_port_type_error(exptype, port))

static Value close_typed_port(Value port, PortType type)
{
    EXPECT_PORT(type, port);
    close_port(port);
    return Qfalse;
}

static Value proc_close_input_port(UNUSED Value env, Value port)
{
    return close_typed_port(port, PORT_INPUT);
}

static Value proc_close_output_port(UNUSED Value env, Value port)
{
    return close_typed_port(port, PORT_OUTPUT);
}

// 6.6.2. Input
static Value eof_new(void)
{
    // an empty object
    return (Value) obj_new(TAG_EOF, sizeof(uintptr_t));
}

static Value get_eof_object(void)
{
    INIT_SINGLETON(eof_object, eof_new());
    return eof_object;
}

static Value iread(FILE *in)
{
    Value datum = parse_datum(in, "<read>");
    if (datum == Qundef)
        return get_eof_object();
    return datum;
}

static Value arg_or_current_port(Value arg, PortType type)
{
    if (arg == Qnil)
        return type == PORT_OUTPUT ? get_current_output_port() : get_current_input_port();
    Value p = car(arg);
    EXPECT_PORT(type, p);
    return p;
}

static Value proc_read(UNUSED Value env, Value args)
{
    EXPECT_ARITY_RANGE(0, 1, args);
    Value port = arg_or_current_port(args, PORT_INPUT);
    EXPECT_ERROR(port);
    return iread(PORT(port)->fp);
}

static Value port_getc(Value port, bool peek)
{
    FILE *fp = PORT(port)->fp;
    int ch = fgetc(fp);
    if (ch == EOF)
        return get_eof_object();
    if (peek)
        ungetc(ch, fp);
    return sch_character_new((uint8_t) ch);
}

static Value proc_read_char(UNUSED Value env, Value args)
{
    EXPECT_ARITY_RANGE(0, 1, args);
    Value port = arg_or_current_port(args, PORT_INPUT);
    EXPECT_ERROR(port);
    return port_getc(port, false);
}

static Value proc_peek_char(UNUSED Value env, Value args)
{
    EXPECT_ARITY_RANGE(0, 1, args);
    Value port = arg_or_current_port(args, PORT_INPUT);
    EXPECT_ERROR(port);
    return port_getc(port, true);
}

static Value proc_eof_object_p(UNUSED Value env, Value obj)
{
    return BOOL_VAL(value_tag_is(obj, TAG_EOF));
}

static Value is_readable_fp(FILE *fp)
{
    int fd = fileno(fp);
    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    struct timeval t = { 0, 0 };
    int ret = select(fd + 1, &fds, NULL, NULL, &t);
    return BOOL_VAL(ret > 0);
}

static Value is_char_input_ready(Value vport)
{
    const Port *port = PORT(vport);
    if (port->string != NULL) // string port
        return Qtrue; // true always
    return is_readable_fp(port->fp);
}

static Value proc_char_ready_p(UNUSED Value env, Value args)
{
    EXPECT_ARITY_RANGE(0, 1, args);
    Value port = arg_or_current_port(args, PORT_INPUT);
    EXPECT_ERROR(port);
    return is_char_input_ready(port);
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
    for (size_t i = 0, len = scary_length(v); i < len; i++) {
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
    case TYPE_CHAR:
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
    case TYPE_CHAR:
        fprintf(f, "%c", CHAR(v));
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

static Value proc_display(UNUSED Value env, Value args)
{
    EXPECT_ARITY_RANGE(1, 2, args);
    Value obj = car(args);
    Value out = arg_or_current_port(cdr(args), PORT_OUTPUT);
    EXPECT_ERROR(out);
    fdisplay(PORT(out)->fp, obj);
    return Qfalse;
}

static Value proc_newline(UNUSED Value env, Value args)
{
    EXPECT_ARITY_RANGE(0, 1, args);
    Value out = arg_or_current_port(args, PORT_OUTPUT);
    EXPECT_ERROR(out);
    fputs("\n", PORT(out)->fp);
    return Qfalse;
}

static Value proc_write_char(UNUSED Value env, Value args)
{
    EXPECT_ARITY_RANGE(1, 2, args);
    Value ch = car(args);
    EXPECT_TYPE(character, ch);
    Value port = arg_or_current_port(cdr(args), PORT_OUTPUT);
    EXPECT_ERROR(port);
    fputc(CHAR(ch), PORT(port)->fp);
    return Qnil;
}

// 6.6.4. System interface
static Value proc_load(UNUSED Value env, Value path)
{
    EXPECT_TYPE(string, path);
    // Current spec: path is always relative
    return load_inner(STRING(path));
}

//
// Extensions from R7RS
//

// (scheme base)
static Value read_string(size_t k, FILE *fp)
{
    if (k == 0)
        return sch_string_new("");
    char buf[k + 1];
    char *p = fgets(buf, sizeof(buf), fp);
    if (p == NULL)
        return get_eof_object();
    return sch_string_new(buf);
}

static Value proc_read_string(UNUSED Value env, Value args)
{
    EXPECT_ARITY_RANGE(1, 2, args);
    size_t k = get_non_negative_int(car(args));
    Value port = arg_or_current_port(cdr(args), PORT_INPUT);
    EXPECT_ERROR(port);
    return read_string(k, PORT(port)->fp);
}

static Value string_port_new(void)
{
    Value v = port_new(NULL, PORT_OUTPUT);
    Port *p = PORT(v);
    p->string = (void *) 1U; // non-NULL dummy value to mark string port
    p->fp = mopen_w(&p->string);
    return v;
}

static Value proc_open_output_string(UNUSED Value env)
{
    return string_port_new();
}

static Value proc_get_output_string(UNUSED Value env, Value port)
{
    EXPECT_PORT(PORT_OUTPUT, port);
    Port *p = PORT(port);
    EXPECT(p->string != NULL, "not a string port");
    fflush(p->fp); // open_memstream() requires flushing
    return sch_string_new(p->string);
}

// (scheme process-context)
static Value proc_exit(UNUSED Value env, Value args)
{
    EXPECT_ARITY_RANGE(0, 1, args);
    exit_status = 0;
    if (args != Qnil) {
        Value obj = car(args);
        if (obj == Qtrue)
            ; // use 0 for code as is
        else if (sch_value_is_integer(obj))
            exit_status = INT(obj);
        else // or #f too
            exit_status = 2; // something failed
    }
    longjmp(jmp_exit, 1);
}

// (scheme lazy)
static Value proc_promise_p(UNUSED Value env, Value obj)
{
    return BOOL_VAL(sch_value_is_promise(obj));
}

//
// Local Extensions
//

static Value syn_defined_p(Value env, Value name)
{
    if (!sch_value_is_symbol(name))
        return Qfalse;
    return BOOL_VAL(env_get(env, name) != Qundef);
}

static Value proc_cputime(UNUSED Value env) // in micro sec
{
    static const int64_t MICRO = 1000*1000;
    struct timespec t;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t);
    int64_t n = t.tv_sec * MICRO + lround(t.tv_nsec / 1000.0);
    return sch_integer_new(n);
}

static Value print_foreach(Value l, ValuePrinter printer)
{
    Value obj = Qfalse;
    for (Value p = l, next; p != Qnil; p = next)  {
        obj = car(p);
        printer(stdout, obj);
        next = cdr(p);
        if (next != Qnil)
            printf(" ");
    }
    puts("");
    return obj;
}

static Value proc_print(UNUSED Value env, Value l)
{
    return print_foreach(l, fdisplay);
}

static Value proc_schaf_environment(UNUSED Value env)
{
    return env_dup(NULL, env_default);
}

static void inspect_string(FILE *f, const char *s)
{
    const char *p = s;
    fprintf(f, "\"");
    for (const char *pbs; (pbs = strchr(p, '\\')) != NULL; p = pbs + 1) {
        fprintf(f, "%.*s", (int) (pbs - p), p);
        fprintf(f, "%s", "\\\\"); // two slashes
    }
    fprintf(f, "%s\"", p);
}

static void inspect_character(FILE *f, uint8_t ch)
{
    fprintf(f, "#\\");
    if (ch == ' ')
        fprintf(f, "space");
    else if (ch == '\n')
        fprintf(f, "newline");
    else
        fprintf(f, "%c", ch);
}

static void inspect_single(FILE *f, Value v)
{
    switch (sch_value_type_of(v)) {
    case TYPE_STRING:
        inspect_string(f, STRING(v));
        break;
    case TYPE_SYMBOL:
        fprintf(f, "'");
        fdisplay_single(f, v);
        break;
    case TYPE_CHAR:
        inspect_character(f, CHAR(v));
        break;
    case TYPE_NULL:
    case TYPE_BOOL:
    case TYPE_INT:
    case TYPE_PROC:
    case TYPE_UNDEF:
    case TYPE_ENV:
    case TYPE_PORT:
    case TYPE_PROMISE:
    case TYPE_EOF:
        fdisplay_single(f, v);
        break;
    case TYPE_PAIR:
    case TYPE_VECTOR:
        bug("invalid type %s", sch_value_to_type_name(v));
    }
}

static void inspect(FILE *f, Value v)
{
    print_object(f, v, Qnil, inspect_single);
}

char *sch_inspect(Value v)
{
    char *s;
    FILE *fp = mopen_w(&s);
    inspect(fp, v);
    fclose(fp);
    return s;
}

static Value proc_p(UNUSED Value env, Value args)
{
    return print_foreach(args, inspect);
}

//
// Interpreter things
//

int sch_fin(void)
{
    scary_free_with(source_free, source_data);
    scary_free_with(free, symbol_names);
    gc_fin();
    return exit_status;
}

void sch_init(const void *sp)
{
    gc_init(sp);

    gc_add_root(&eof_object);
    gc_add_root(&current_input_port);
    gc_add_root(&current_output_port);
    gc_add_root(&inner_winders);
    gc_add_root(&inner_continuation);

    static char basedir[PATH_MAX];
    load_basedir = getcwd(basedir, sizeof(basedir));
    symbol_names = scary_new(sizeof(char *));
#define DEF_SYMBOL(var, name) SYM_##var = sch_symbol_new(name)
    DEF_SYMBOL(ELSE, "else");
    DEF_SYMBOL(QUOTE, "quote");
    DEF_SYMBOL(QUASIQUOTE, "quasiquote");
    DEF_SYMBOL(UNQUOTE, "unquote");
    DEF_SYMBOL(UNQUOTE_SPLICING, "unquote-splicing");
    DEF_SYMBOL(RARROW, "=>");
    source_data = scary_new(sizeof(Source *));

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
    // 4.2.5. Delayed evaluation
    define_syntax(e, "delay", syn_delay, 1);
    // 4.2.6. Quasiquotation
    define_syntax(e, "quasiquote", syn_quasiquote, 1);
    define_syntax(e, "unquote", syn_unquote, 1);
    define_syntax(e, "unquote-splicing", syn_unquote_splicing, 1);
    // 4.3. Macros
    // 4.3.1. Binding constructs for syntactic keywords
    //- let-syntax
    //- letrec-syntax
    // 4.3.2. Pattern language
    //- syntax-rules

    // 5. Program structure

    // 5.2. Definitions
    define_syntax(e, "define", syn_define, -1);
    // 5.3. Syntax definitions
    //- define-syntax
    env_null = env_dup("null", e);
    gc_add_root(&env_null);

    // 6. Standard procedures

    // 6.1. Equivalence predicates
    define_procedure(e, "eqv?", proc_eq, 2); // alias
    define_procedure(e, "eq?", proc_eq, 2);
    define_procedure(e, "equal?", proc_equal, 2);
    // 6.2. Numbers
    // 6.2.5. Numerical operations
    define_procedure(e, "number?", proc_integer_p, 1); // alias
    //- complex?
    //- rational?
    //- real?
    define_procedure(e, "integer?", proc_integer_p, 1);
    //- exact?
    //- inexact?
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
    define_procedure(e, "gcd", proc_gcd, -1);
    define_procedure(e, "lcm", proc_lcm, -1);
    //- numerator
    //- denominator
    //- floor
    //- ceiling
    //- truncate
    //- round
    //- rationalize
    //- exp
    //- log
    //- sin
    //- cos
    //- tan
    //- asin
    //- acos
    //- atan
    //- sqrt
    define_procedure(e, "expt", proc_expt, 2);
    //- make-rectangular
    //- make-polar
    //- real-part
    //- imag-part
    //- magnitude
    //- angle
    //- exact->inexact
    //- inexact->exact
    // 6.2.6. Numerical input and output
    define_procedure(e, "number->string", proc_number_to_string, -1);
    define_procedure(e, "string->number", proc_string_to_number, -1);
    // 6.3. Other data types
    // 6.3.1. Booleans
    define_procedure(e, "not", proc_not, 1);
    define_procedure(e, "boolean?", proc_boolean_p, 1);
    // 6.3.2. Pairs and lists
    define_procedure(e, "pair?", proc_pair_p, 1);
    define_procedure(e, "cons", proc_cons, 2);
    define_procedure(e, "car", proc_car, 1);
    define_procedure(e, "cdr", proc_cdr, 1);
    define_procedure(e, "set-car!", proc_set_car, 2);
    define_procedure(e, "set-cdr!", proc_set_cdr, 2);
#define DEFUN_CXR(x, y) define_procedure(e, "c" #x #y "r", proc_c##x##y##r, 1);
    CXRS(DEFUN_CXR); // registers 28 procedures
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
    define_procedure(e, "assoc", proc_assoc, 2);
    // 6.3.3. Symbols
    define_procedure(e, "symbol?", proc_symbol_p, 1);
    define_procedure(e, "symbol->string", proc_symbol_to_string, 1);
    define_procedure(e, "string->symbol", proc_string_to_symbol, 1);
    // 6.3.4. Characters
    define_procedure(e, "char?", proc_char_p, 1);
    define_procedure(e, "char=?", proc_char_eq_p, 2);
    define_procedure(e, "char<?", proc_char_lt_p, 2);
    define_procedure(e, "char>?", proc_char_gt_p, 2);
    define_procedure(e, "char<=?", proc_char_le_p, 2);
    define_procedure(e, "char>=?", proc_char_ge_p, 2);
    define_procedure(e, "char-ci=?", proc_char_ci_eq_p, 2);
    define_procedure(e, "char-ci<?", proc_char_ci_lt_p, 2);
    define_procedure(e, "char-ci>?", proc_char_ci_gt_p, 2);
    define_procedure(e, "char-ci<=?", proc_char_ci_le_p, 2);
    define_procedure(e, "char-ci>=?", proc_char_ci_ge_p, 2);
    define_procedure(e, "char-alphabetic?", proc_char_alphabetic_p, 1);
    define_procedure(e, "char-numeric?", proc_char_numeric_p, 1);
    define_procedure(e, "char-whitespace?", proc_char_whitespace_p, 1);
    define_procedure(e, "char-upper-case?", proc_char_upper_case_p, 1);
    define_procedure(e, "char-lower-case?", proc_char_lower_case_p, 1);
    define_procedure(e, "char->integer", proc_char_to_integer, 1);
    define_procedure(e, "integer->char", proc_integer_to_char, 1);
    define_procedure(e, "char-upcase", proc_char_upcase, 1);
    define_procedure(e, "char-downcase", proc_char_downcase, 1);
    // 6.3.5. Strings
    define_procedure(e, "string?", proc_string_p, 1);
    define_procedure(e, "make-string", proc_make_string, -1);
    define_procedure(e, "string", proc_string, -1);
    define_procedure(e, "string-length", proc_string_length, 1);
    define_procedure(e, "string-ref", proc_string_ref, 2);
    define_procedure(e, "string-set!", proc_string_set, 3);
    define_procedure(e, "string=?", proc_string_eq, 2);
    define_procedure(e, "string-ci=?", proc_string_ci_eq, 2);
    define_procedure(e, "string<?", proc_string_lt, 2);
    define_procedure(e, "string>?", proc_string_gt, 2);
    define_procedure(e, "string<=?", proc_string_le, 2);
    define_procedure(e, "string>=?", proc_string_ge, 2);
    define_procedure(e, "string-ci<?", proc_string_ci_lt, 2);
    define_procedure(e, "string-ci>?", proc_string_ci_gt, 2);
    define_procedure(e, "string-ci<=?", proc_string_ci_le, 2);
    define_procedure(e, "string-ci>=?", proc_string_ci_ge, 2);
    define_procedure(e, "substring", proc_substring, 3);
    define_procedure(e, "string-append", proc_string_append, -1);
    define_procedure(e, "string->list", proc_string_to_list, 1);
    define_procedure(e, "list->string", proc_list_to_string, 1);
    define_procedure(e, "string-copy", proc_string_copy, 1);
    define_procedure(e, "string-fill!", proc_string_fill, 2);
    // 6.3.6. Vectors
    define_procedure(e, "vector?", proc_vector_p, 1);
    define_procedure(e, "make-vector", proc_make_vector, -1);
    define_procedure(e, "vector", proc_vector, -1);
    define_procedure(e, "vector-length", proc_vector_length, 1);
    define_procedure(e, "vector-ref", proc_vector_ref, 2);
    define_procedure(e, "vector-set!", proc_vector_set, 3);
    define_procedure(e, "vector->list", proc_vector_to_list, 1);
    define_procedure(e, "list->vector", proc_list_to_vector, 1);
    define_procedure(e, "vector-fill!", proc_vector_fill, 2);

    // 6.4. Control features
    define_procedure(e, "procedure?", proc_procedure_p, 1);
    define_procedure(e, "apply", proc_apply, -1);
    define_procedure(e, "map", proc_map, -1);
    define_procedure(e, "for-each", proc_for_each, -1);
    define_procedure(e, "force", proc_force, 1);
    define_procedure(e, "call/cc", proc_callcc, 1); // alias
    define_procedure(e, "call-with-current-continuation", proc_callcc, 1);
    define_procedure(e, "values", proc_values, -1);
    define_procedure(e, "call-with-values", proc_call_with_values, 2);
    define_procedure(e, "dynamic-wind", proc_dynamic_wind, 3);
    // 6.5. Eval
    define_procedure(e, "eval", proc_eval, 2);
    define_procedure(e, "scheme-report-environment", proc_scheme_report_environment, 1);
    define_procedure(e, "null-environment", proc_null_environment, 1);
    define_procedure(e, "interaction-environment", proc_interaction_environment, 0);
    // 6.6. Input and output
    // 6.6.1. Ports
    define_procedure(e, "call-with-input-file", proc_call_with_input_file, 2);
    define_procedure(e, "call-with-output-file", proc_call_with_output_file, 2);
    define_procedure(e, "input-port?", proc_input_port_p, 1);
    define_procedure(e, "output-port?", proc_output_port_p, 1);
    define_procedure(e, "current-input-port", proc_current_input_port, 0);
    define_procedure(e, "current-output-port", proc_current_output_port, 0);
    define_procedure(e, "with-input-from-file", proc_with_input_from_file, 2);
    define_procedure(e, "with-output-to-file", proc_with_output_to_file, 2);
    define_procedure(e, "open-input-file", proc_open_input_file, 1);
    define_procedure(e, "open-output-file", proc_open_output_file, 1);
    define_procedure(e, "close-input-port", proc_close_input_port, 1);
    define_procedure(e, "close-output-port", proc_close_output_port, 1);
    // 6.6.2. Input
    define_procedure(e, "read", proc_read, -1);
    define_procedure(e, "read-char", proc_read_char, -1);
    define_procedure(e, "peek-char", proc_peek_char, -1);
    define_procedure(e, "eof-object?", proc_eof_object_p, 1);
    define_procedure(e, "char-ready?", proc_char_ready_p, -1);
    // 6.6.3. Output
    //- write
    define_procedure(e, "display", proc_display, -1);
    define_procedure(e, "newline", proc_newline, -1);
    define_procedure(e, "write-char", proc_write_char, -1);
    // 6.6.4. System interface
    define_procedure(e, "load", proc_load, 1);

    env_r5rs = env_dup("r5rs", e);
    gc_add_root(&env_r5rs);

    // Extensions from R7RS
    // (scheme base)
    define_procedure(e, "close-port", proc_close_port, 1);
    define_procedure(e, "read-string", proc_read_string, -1);
    define_procedure(e, "open-output-string", proc_open_output_string, 0);
    define_procedure(e, "get-output-string", proc_get_output_string, 1);
    // (scheme process-context)
    define_procedure(e, "exit", proc_exit, -1);
    // (scheme lazy)
    define_procedure(e, "promise?", proc_promise_p, 1);

    // Local Extensions
    define_syntax(e, "_defined?", syn_defined_p, 1);
    define_procedure(e, "_cputime", proc_cputime, 0);
    define_procedure(e, "p", proc_p, -1);
    define_procedure(e, "print", proc_print, -1); // like Gauche
    define_procedure(e, "schaf-environment", proc_schaf_environment, 0);

    env_default = env_dup("default", e);
    gc_add_root(&env_default);
}
