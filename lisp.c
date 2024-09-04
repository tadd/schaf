#include <ctype.h>
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

#include "lisp.h"
#include "utils.h"

//
// Types
//

static const char *TYPE_NAMES[] = {
    [TYPE_BOOL] = "boolean",
    [TYPE_INT] = "integer",
    [TYPE_SYMBOL] = "symbol",
    [TYPE_UNDEF] = "undef",
    [TYPE_PAIR] = "pair",
    [TYPE_STR] = "string",
    [TYPE_CFUNC] = "C function",
    [TYPE_SPECIAL] = "special form",
    [TYPE_CLOSURE] = "closure",
    [TYPE_CONTINUATION] = "continuation",
};

typedef enum { // has the same values as Type
    TAG_PAIR    = TYPE_PAIR,
    TAG_STR     = TYPE_STR,
    TAG_CFUNC   = TYPE_CFUNC,
    TAG_SPECIAL = TYPE_SPECIAL, // almost a C Function
    TAG_CLOSURE = TYPE_CLOSURE,
    TAG_CONTINUATION = TYPE_CONTINUATION,
} ValueTag;

typedef struct Pair {
    ValueTag tag; // common
    Value car, cdr;
} Pair;

typedef struct {
    ValueTag tag;
    const char *body;
} String;

typedef struct {
    ValueTag tag;
    int64_t arity;
} Function;

typedef struct {
    Function func;
    cfunc_t cfunc;
} CFunc;

typedef struct {
    Function func;
    Value env;
    Value params;
    Value body;
} Closure;

typedef struct {
    Function func;
    volatile void *sp;
    void *shelter;
    size_t shelter_len;
    jmp_buf state;
    Value retval;
} Continuation;

#define VALUE_TAG(v) (*(ValueTag*)(v))
#define PAIR(v) ((Pair *) v)
#define STRING(v) ((String *) v)
#define FUNCTION(v) ((Function *) v)
#define CFUNC(v) ((CFunc *) v)
#define CLOSURE(v) ((Closure *) v)
#define CONTINUATION(v) ((Continuation *) v)
#define OF_BOOL(v) ((v) ? Qtrue : Qfalse)

// singletons
static const Pair PAIR_NIL = { .tag = TAG_PAIR, .car = 0, .cdr = 0 };
// Value (uintptr_t):
//   0b....000 Pointer (Unchangeable pattern!)
//   0b......1 Integer
//   0b...1110 Symbol
//   0b0..0010 #f
//   0b0..0100 #t
//   0b0..0110 <undef>
static const uintptr_t FLAG_NBIT = 4U;
static const uintptr_t MASK_IMMEDIATE = 0b1111U;
static const uintptr_t FLAG_SYMBOL    = 0b1110U;
const Value Qnil = (Value) &PAIR_NIL;
const Value Qfalse = 0b0010U;
const Value Qtrue  = 0b0100U;
const Value Qundef = 0b0110U; // may be an error or something

static const int64_t CFUNCARG_MAX = 7;

//
// Runtime-locals (aka global variables)
//

static Value toplevel_environment = Qnil; // alist of ('symbol . <value>)
static Value symbol_names = Qnil; // ("name0" "name1" ...)
static Value SYM_ELSE, SYM_QUOTE, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING;
static const volatile void *stack_base = NULL;
#define INIT_STACK() void *basis; stack_base = &basis
static const char *load_basedir = NULL;

//
// value_is_*: Type Checks
//

static inline uintptr_t flags(Value v)
{
    return v & MASK_IMMEDIATE;
}

inline bool value_is_int(Value v)
{
    return v & 1U;
}

inline bool value_is_symbol(Value v)
{
    return flags(v) == FLAG_SYMBOL;
}

static inline bool is_immediate(Value v)
{
    return v & 0b111U; // for 64 bit machine
}

static inline bool value_tag_is(Value v, ValueTag expected)
{
    return !is_immediate(v) && VALUE_TAG(v) == expected;
}

inline bool value_is_string(Value v)
{
    return value_tag_is(v, TAG_STR);
}

inline bool value_is_pair(Value v)
{
    return value_tag_is(v, TAG_PAIR);
}

static inline bool value_is_atom(Value v)
{
    return !value_is_pair(v);
}

inline bool value_is_nil(Value v)
{
    return v == Qnil;
}

static Type immediate_type_of(Value v)
{
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
    if (is_immediate(v))
        return immediate_type_of(v);
    ValueTag t = VALUE_TAG(v);
    switch (t) {
    case TAG_STR:
    case TAG_PAIR:
    case TAG_CFUNC:
    case TAG_SPECIAL:
    case TAG_CLOSURE:
    case TAG_CONTINUATION:
        return (Type) t;
    }
    UNREACHABLE();
}

inline const char *value_type_to_string(Type t)
{
    return TYPE_NAMES[t];
}

// value_to_*: Convert internal data to external plain C

inline int64_t value_to_int(Value v)
{
    return (int64_t) v >> 1U;
}

inline Symbol value_to_symbol(Value v)
{
    return (Symbol) (v >> FLAG_NBIT);
}

static Symbol intern(const char *s);
static const char *unintern(Symbol sym);

inline const char *value_to_string(Value v)
{
    if (value_is_symbol(v))
        return unintern(value_to_symbol(v));
    return STRING(v)->body;
}

// value_of_*: Convert external plain C data to internal

inline Value value_of_int(int64_t i)
{
    return (Value) i << 1U | 1U;
}

inline Value value_of_symbol(const char *s)
{
    Symbol sym = intern(s);
    return (Value) (sym << FLAG_NBIT | FLAG_SYMBOL);
}

static void *obj_new(size_t size, ValueTag t)
{
    void *p = xmalloc(size);
    VALUE_TAG(p) = t;
    return p;
}

Value value_of_string(const char *s)
{
    String *str = obj_new(sizeof(String), TAG_STR);
    str->body = xstrdup(s);
    return (Value) str;
}

static Value value_of_cfunc(cfunc_t cfunc, int64_t arity)
{
    CFunc *f = obj_new(sizeof(CFunc), TAG_CFUNC);
    f->func.arity = arity;
    f->cfunc = cfunc;
    return (Value) f;
}

static Value value_of_special(cfunc_t cfunc, int64_t arity)
{
    Value sp = value_of_cfunc(cfunc, arity);
    VALUE_TAG(sp) = TAG_SPECIAL;
    return sp;
}

static Value value_of_closure(Value env, Value params, Value body)
{
    Closure *f = obj_new(sizeof(Closure), TAG_CLOSURE);
    f->func.arity = (value_type_of(params) == TYPE_PAIR) ? length(params) : -1;
    f->env = env;
    f->params = params;
    f->body = body;
    return (Value) f;
}

// `cons` is well-known name than "value_of_pair"
Value cons(Value car, Value cdr)
{
    Pair *p = obj_new(sizeof(Pair), TAG_PAIR);
    p->car = car;
    p->cdr = cdr;
    return (Value) p;
}

//
// Errors
//

#define error(fmt, ...) \
    error("%s:%d of %s: " fmt, __FILE__, __LINE__, __func__ __VA_OPT__(,) __VA_ARGS__)

static jmp_buf jmp_runtime_error, jmp_parse_error;
static char errmsg[BUFSIZ];

ATTR(noreturn)
static void runtime_error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(errmsg, sizeof(errmsg), fmt, ap);
    va_end(ap);
    longjmp(jmp_runtime_error, 1);
}

const char *error_message(void)
{
    return errmsg;
}

static void expect_type(const char *header, Type expected, Value v)
{
    Type t = value_type_of(v);
    if (t == expected)
        return;
    runtime_error("type error in %s: expected %s but got %s",
                  header, value_type_to_string(expected), value_type_to_string(t));
}
#define expect_type_twin(h, t, x, y) expect_type(h, t, x), expect_type(h, t, y)

static void expect_type_or(const char *header, Type e1, Type e2, Value v)
{
    Type t = value_type_of(v);
    if (t == e1 || t == e2)
        return;
    runtime_error("type error in %s: expected %s or %s but got %s",
                  header, value_type_to_string(e1), value_type_to_string(e2),
                  value_type_to_string(t));
}

// Lists: prepare for parsing

static inline Value list1(Value x)
{
    return cons(x, Qnil);
}

static Value append_at(Value last, Value elem)
{
    Value p = list1(elem);
    if (last != Qnil)
        PAIR(last)->cdr = p;
    return p;
}

Value list(Value v, ...)
{
    Value l = Qnil, last = l;
    va_list ap;
    va_start(ap, v);
    for (; v != Qundef; v = va_arg(ap, Value)) {
        last = append_at(last, v);
        if (l == Qnil)
            l = last;
    }
    va_end(ap);
    return l;
}

//
// Parse
//

typedef enum {
    TOK_TYPE_LPAREN,
    TOK_TYPE_RPAREN,
    TOK_TYPE_QUOTE,
    TOK_TYPE_GRAVE,
    TOK_TYPE_COMMA,
    TOK_TYPE_SPLICE,
    TOK_TYPE_INT,
    TOK_TYPE_DOT,
    TOK_TYPE_STR,
    TOK_TYPE_IDENT,
    TOK_TYPE_CONST,
    TOK_TYPE_EOF
} TokenType;

typedef struct {
    TokenType type;
    Value value;
} Token;

#define TOK(t) { .type = TOK_TYPE_ ## t }
// singletons
static const Token
    TOK_LPAREN = TOK(LPAREN),
    TOK_RPAREN = TOK(RPAREN),
    TOK_QUOTE = TOK(QUOTE),
    TOK_GRAVE = TOK(GRAVE),
    TOK_COMMA = TOK(COMMA),
    TOK_SPLICE = TOK(SPLICE),
    TOK_DOT = TOK(DOT),
    TOK_EOF = TOK(EOF);
// and ctor
#define TOK_V(t, v) ((Token) { .type = TOK_TYPE_ ## t, .value = v })
#define TOK_INT(i) TOK_V(INT, value_of_int(i))
#define TOK_STR(s) TOK_V(STR, value_of_string(s))
#define TOK_IDENT(s) TOK_V(IDENT, value_of_symbol(s))
#define TOK_CONST(c) TOK_V(CONST, c)

typedef struct {
    FILE *in;
    Token prev_token;
} Parser;

#define parse_error(p, exp, act, ...) do { \
        int64_t line, col; \
        get_line_column(p, &line, &col); \
        memcpy(jmp_runtime_error, jmp_parse_error, sizeof(jmp_buf)); \
        runtime_error("on %"PRId64":%"PRId64": expected %s but got " act, line, col, \
                      exp __VA_OPT__(,) __VA_ARGS__); \
    } while (0)

static void get_line_column(Parser *p, int64_t *line, int64_t *col)
{
    FILE *in = p->in;
    int64_t loc = ftell(in);
    if (loc < 0) {
        *line = *col = 0;
        return;
    }
    rewind(in);
    int64_t nline = 1;
    int64_t last_newline = 0;
    for (int64_t i = 0; i < loc; i++) {
        if (fgetc(in) == '\n') {
            nline++;
            last_newline = i;
        }
    }
    *line = nline;
    *col = loc - last_newline;
}

static Token get_token_int(Parser *p, int sign)
{
    int64_t i;
    int n = fscanf(p->in, "%"SCNd64, &i);
    if (n != 1)
        parse_error(p, "integer", "invalid string");
    return TOK_INT(sign * i);
}

static Token get_token_string(Parser *p)
{
    char buf[BUFSIZ], *pbuf = buf, *end = pbuf + sizeof(buf) - 2;
    for (;;) {
        int c = fgetc(p->in);
        if (c == '"')
            break;
        if (c == '\\') {
            c = fgetc(p->in);
            if (c != '\\' && c != '"')
                parse_error(p, "'\\' or '\"' in string literal", "'%c'", c);
        }
        if (pbuf == end)
            parse_error(p, "string literal", "too long: \"%s...\"", pbuf);
        *pbuf++ = c;
    }
    *pbuf = '\0';
    return TOK_STR(buf);
}

static Symbol intern(const char *name)
{
    int64_t i;
    Value l = symbol_names;
    Value last = Qnil;
    // find
    for (i = 0; l != Qnil; l = cdr(l), i++) {
        Value v = car(l);
        if (strcmp(STRING(v)->body, name) == 0)
            return i;
        last = l;
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
    if (name == NULL) // fatal; known symbol should have name
        error("symbol %lu not found", sym);
    return name;
}

static inline bool is_initial(int c)
{
    return isalpha(c) ||
        c == '*' || c == '/' || c == '<' || c == '=' || c == '>' || c == '_';
}

static inline bool is_subsequent(int c)
{
    return isalpha(c) || isdigit(c) ||
        c == '*' || c == '/' || c == '-' || c == '.' || c == '!' || c == '=' ||
        c == '?';
}

static Token get_token_ident(Parser *p)
{
    char buf[BUFSIZ], *s = buf, *end = s + sizeof(buf);
    int c = fgetc(p->in);

    *s++ = c;
    for (;;) {
        c = fgetc(p->in);
        if (!is_subsequent(c))
            break;
        *s++ = c;
        if (s == end)
            parse_error(p, "identifier", "too long");
    }
    ungetc(c, p->in);
    *s = '\0';
    return TOK_IDENT(buf);
}

static Token get_token_after_sign(Parser *p, int csign)
{
    int c = fgetc(p->in);
    int dig = isdigit(c);
    ungetc(c, p->in);
    if (dig) {
        int sign = csign == '-' ? -1 : 1;
        return get_token_int(p, sign);
    }
    char ident[] = { csign, '\0' };
    return TOK_IDENT(ident);
}

static void skip_token_atmosphere(Parser *p)
{
    int c;
    for (;;) {
        c = fgetc(p->in);
        if (isspace(c))
            continue; // skip
        if (c == ';') {
            do {
                c = fgetc(p->in);
            } while (c != '\n' && c != EOF);
            continue;
        }
        break;
    }
    ungetc(c, p->in);
}

static Token get_token_comma_or_splice(Parser *p)
{
    int c = fgetc(p->in);
    if (c == '@')
        return TOK_SPLICE;
    ungetc(c, p->in);
    return TOK_COMMA;
}

static Token get_token(Parser *p)
{
    if (p->prev_token.type != TOK_TYPE_EOF)  {
        Token t = p->prev_token;
        p->prev_token = TOK_EOF;
        return t;
    }

    skip_token_atmosphere(p);
    int c = fgetc(p->in);
    switch (c) {
    case '(':
        return TOK_LPAREN;
    case ')':
        return TOK_RPAREN;
    case '\'':
        return TOK_QUOTE;
    case '`':
        return TOK_GRAVE;
    case ',':
        return get_token_comma_or_splice(p);
    case '.':
        return TOK_DOT;
    case '"':
        return get_token_string(p);
    case '#':
        c = fgetc(p->in);
        if (c == 't')
            return TOK_CONST(Qtrue);
        if (c == 'f')
            return TOK_CONST(Qfalse);
        parse_error(p, "constants", "#%c", c);
    case EOF:
        return TOK_EOF;
    default:
        break;
    }
    if (c == '-' || c == '+')
        return get_token_after_sign(p, c);
    if (isdigit(c)) {
        ungetc(c, p->in);
        return get_token_int(p, 1);
    }
    if (is_initial(c)) {
        ungetc(c, p->in);
        return get_token_ident(p);
    }
    parse_error(p, "valid char", "'%c'", c);
}

static void unget_token(Parser *p, Token t)
{
    p->prev_token = t;
}

inline Value car(Value v)
{
    return PAIR(v)->car;
}

inline Value cdr(Value v)
{
    return PAIR(v)->cdr;
}

#define DEF_CXR(x, y) Value c##x##y##r(Value v) { return c##x##r(c##y##r(v)); }
#define DEF_CXR1(x) DEF_CXR(a, x) DEF_CXR(d, x)
#define DEF_CXR2(x) DEF_CXR1(a ## x) DEF_CXR1(d ## x)
#define DEF_CXR3(x) DEF_CXR2(a ## x) DEF_CXR2(d ## x)
#define DEF_CXR4(x) DEF_CXR3(a ## x) DEF_CXR3(d ## x)
#define DEF_CXRS() DEF_CXR2() DEF_CXR3() DEF_CXR4()

DEF_CXRS()

static Value parse_expr(Parser *p);

static const char *token_stringify(Token t)
{
    static char buf[BUFSIZ];

    switch (t.type) {
    case TOK_TYPE_LPAREN:
        return "(";
    case TOK_TYPE_RPAREN:
        return ")";
    case TOK_TYPE_QUOTE:
        return "'";
    case TOK_TYPE_GRAVE:
        return "`";
    case TOK_TYPE_COMMA:
        return ",";
    case TOK_TYPE_SPLICE:
        return ",@";
    case TOK_TYPE_DOT:
        return ".";
    case TOK_TYPE_INT:
        snprintf(buf, sizeof(buf), "%"PRId64, value_to_int(t.value));
        break;
    case TOK_TYPE_IDENT:
        return value_to_string(t.value);
    case TOK_TYPE_STR:
        snprintf(buf, sizeof(buf), "\"%s\"", STRING(t.value)->body);
        break;
    case TOK_TYPE_CONST:
        return t.value == Qtrue ? "#t" : "#f";
    case TOK_TYPE_EOF:
        return "EOF";
    }
    return buf;
}

static Value parse_dotted_pair(Parser *p, Value l, Value last)
{
    if (l == Qnil)
        parse_error(p, "expression", "'.'");
    Value e = parse_expr(p);
    Token t = get_token(p);
    if (t.type != TOK_TYPE_RPAREN)
        parse_error(p, "')'", "'%s'", token_stringify(t));
    PAIR(last)->cdr = e;
    return l;
}

static Value parse_list(Parser *p)
{
    Value l = Qnil, last = Qnil;
    for (;;) {
        Token t = get_token(p);
        if (t.type == TOK_TYPE_RPAREN)
            break;
        if (t.type == TOK_TYPE_EOF)
            parse_error(p, "')'", "'%s'", token_stringify(t));
        if (t.type == TOK_TYPE_DOT)
            return parse_dotted_pair(p, l, last);
        unget_token(p, t);
        Value e = parse_expr(p);
        last = append_at(last, e);
        if (l == Qnil)
            l = last;
    }
    return l;
}

static inline Value list2(Value x, Value y)
{
    return cons(x, cons(y, Qnil));
}

static Value parse_quoted(Parser *p, Value sym)
{
    Value e = parse_expr(p);
    if (e == Qundef)
        parse_error(p, "expression", "'EOF'");
    return list2(sym, e);
}

static Value parse_expr(Parser *p)
{
    Token t = get_token(p);
    switch (t.type) {
    case TOK_TYPE_LPAREN:
        return parse_list(p); // parse til ')'
    case TOK_TYPE_RPAREN:
        parse_error(p, "expression", "')'");
    case TOK_TYPE_QUOTE:
        return parse_quoted(p, SYM_QUOTE);
    case TOK_TYPE_GRAVE:
        return parse_quoted(p, SYM_QUASIQUOTE);
    case TOK_TYPE_COMMA:
        return parse_quoted(p, SYM_UNQUOTE);
    case TOK_TYPE_SPLICE:
        return parse_quoted(p, SYM_UNQUOTE_SPLICING);
    case TOK_TYPE_DOT:
        parse_error(p, "expression", "'.'");
    case TOK_TYPE_STR:
    case TOK_TYPE_INT:
    case TOK_TYPE_CONST:
    case TOK_TYPE_IDENT:
        return t.value;
    case TOK_TYPE_EOF:
        break;
    }
    return Qundef;
}

static Parser *parser_new(FILE *in)
{
    Parser *p = xmalloc(sizeof(Parser));
    p->in = in;
    p->prev_token = TOK_EOF; // we use this since we never postpone EOF things
    return p;
}

int64_t length(Value list)
{
    int64_t l = 0;
    for (; list != Qnil; list = cdr(list))
        l++;
    return l;
}

static void expect_arity_range(const char *func, int64_t min, int64_t max, Value args)
{
    int64_t actual = length(args);
    if (min <= actual && (max == -1 || actual <= max))
        return;
    runtime_error("%s: wrong number of arguments: expected %"PRId64"..%"PRId64" but got %"PRId64,
                  func, min, max, actual);
}

static void expect_arity(int64_t expected, Value args)
{
    int64_t actual = length(args);
    if (expected < 0 || expected == actual)
        return;
    runtime_error("wrong number of arguments: expected %"PRId64" but got %"PRId64,
                  expected, actual);
}

static Value apply_cfunc(Value *env, Value func, Value args)
{
    Value a[CFUNCARG_MAX];
    CFunc *cf = CFUNC(func);
    int64_t n = cf->func.arity;
    Value arg = args;
    for (int i = 0; i < n; i++) {
        a[i] = car(arg);
        arg = cdr(arg);
    }
    cfunc_t f = cf->cfunc;

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
    case 4:
        return (*f)(env, a[0], a[1], a[2], a[3]);
    case 5:
        return (*f)(env, a[0], a[1], a[2], a[3], a[4]);
    case 6:
        return (*f)(env, a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7:
        return (*f)(env, a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    default:
        error("arity too large: %"PRId64, n);
    }
#if defined(__clang__) && __clang_major__ >= 15
#pragma clang diagnostic pop
#endif
}

static Value ieval(Value *env, Value v); // internal
static Value eval_body(Value *env, Value body);

static Value append2(Value l1, Value l2)
{
    if (l2 == Qnil)
        return l1;
    if (l1 == Qnil)
        return l2;

    Value ret = Qnil, prev = Qnil;
    for (Value h = l1; h != Qnil; h = cdr(h)) {
        Value curr = list1(car(h));
        if (ret == Qnil)
            ret = curr;
        if (prev != Qnil)
            PAIR(prev)->cdr = curr;
        prev = curr;
    }
    PAIR(prev)->cdr = l2;
    return ret;
}

inline static void env_put(Value *env, Value name, Value val)
{
    *env = cons(cons(name, val), *env);
}

static Value apply_closure(Value *env, Value func, Value args)
{
    Closure *cl = CLOSURE(func);
    int64_t arity = cl->func.arity;
    Value clenv = append2(cl->env, *env), params = cl->params;
    if (arity == -1)
        env_put(&clenv, params, args);
    else {
        for (; args != Qnil; args = cdr(args), params = cdr(params))
            env_put(&clenv, car(params), car(args));
    }
    return eval_body(&clenv, cl->body);
}

ATTR(noreturn) ATTR(noinline)
static void jump(Continuation *cont)
{
    memcpy((void *) cont->sp, cont->shelter, cont->shelter_len);
    longjmp(cont->state, 1);
}

#define GET_SP(p) volatile void *p = &p

ATTR(noreturn)
static void apply_continuation(Value f, Value args)
{
    GET_SP(sp);
    Continuation *cont = CONTINUATION(f);
    cont->retval = car(args);
    int64_t d = sp - cont->sp;
    if (d < 1)
        d = 1;
    volatile uint8_t pad[d];
    pad[0] = pad[d-1] = 0; // avoid unused
    jump(cont);
}

static bool is_procedure_type(Type t)
{
    switch (t) {
    case TYPE_SPECIAL:
    case TYPE_CFUNC:
    case TYPE_CLOSURE:
    case TYPE_CONTINUATION:
        return true;
    default:
        return false;
    }
}

static Type expect_applicative(Value v)
{
    Type t = value_type_of(v);
    if (is_procedure_type(t))
        return t;
    runtime_error("type error in (eval): expected applicative but got %s",
                  value_type_to_string(t));
}

static Value apply(Value *env, Value func, Value args)
{
    ValueTag tag = (ValueTag) expect_applicative(func);
    expect_arity(FUNCTION(func)->arity, args);
    switch (tag) {
    case TAG_SPECIAL:
    case TAG_CFUNC:
        return apply_cfunc(env, func, args);
    case TAG_CLOSURE:
        return apply_closure(env, func, args);
    case TAG_CONTINUATION:
        apply_continuation(func, args); // no return!
    default:
        UNREACHABLE();
    }
}

static Value assq(Value key, Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        Value entry = car(p);
        if (value_is_pair(entry) && car(entry) == key)
            return entry;
    }
    return Qfalse;
}

static void expect_cfunc_arity(int64_t actual)
{
    if (actual <= CFUNCARG_MAX)
        return;
    error("arity too large: expected ..%"PRId64" but got %"PRId64,
          CFUNCARG_MAX, actual);
}

static void define_special(Value *env, const char *name, cfunc_t cfunc, int64_t arity)
{
    expect_cfunc_arity(arity);
    env_put(env, value_of_symbol(name), value_of_special(cfunc, arity));
}

static void define_function(Value *env, const char *name, cfunc_t cfunc, int64_t arity)
{
    expect_cfunc_arity(arity);
    env_put(env, value_of_symbol(name), value_of_cfunc(cfunc, arity));
}

static Value lookup(Value env, Value name)
{
    Value found = assq(name, env);
    if (found == Qfalse)
        runtime_error("unbound variable: %s", value_to_string(name));
    return cdr(found);
}

static void fdisplay(FILE* f, Value v);

static void display_list(FILE *f, Value v)
{
    for (;;) {
        Pair *p = PAIR(v);
        fdisplay(f, p->car);
        v = p->cdr;
        if (v == Qnil)
            break;
        fprintf(f, " ");
        if (value_is_atom(v)) {
            fprintf(f, ". ");
            fdisplay(f, v);
            break;
        }
    }
}

static void fdisplay(FILE* f, Value v)
{
    switch (value_type_of(v)) {
    case TYPE_BOOL:
        fprintf(f, "%s", v == Qtrue ? "#t" : "#f");
        break;
    case TYPE_INT:
        fprintf(f, "%"PRId64, value_to_int(v));
        break;
    case TYPE_SYMBOL:
        fprintf(f, "'%s", value_to_string(v));
        break;
    case TYPE_PAIR:
        fprintf(f, "(");
        if (v != Qnil)
            display_list(f, v);
        fprintf(f, ")");
        break;
    case TYPE_STR:
        fprintf(f, "%s", value_to_string(v));
        break;
    case TYPE_CFUNC:
        fprintf(f, "<c-function>");
        break;
    case TYPE_SPECIAL:
        fprintf(f, "<special>");
        break;
    case TYPE_CLOSURE:
        fprintf(f, "<closure>");
        break;
    case TYPE_CONTINUATION:
        fprintf(f, "<continuation>");
        break;
    case TYPE_UNDEF:
        fprintf(f, "<undef>");
        break;
    }
}

void display(Value v)
{
    fdisplay(stdout, v);
}

char *stringify(Value v)
{
    char *s;
    size_t size;
    FILE *stream = open_memstream(&s, &size);
    if (stream == NULL)
        return NULL;
    fdisplay(stream, v);
    fclose(stream);
    return s;
}

static Value reverse(Value v)
{
    Value l = Qnil;
    for (; v != Qnil; v = cdr(v))
        l = cons(car(v), l);
    return l;
}

static Value iparse(FILE *in)
{
    if (setjmp(jmp_parse_error) != 0)
        return Qundef;
    Parser *p = parser_new(in);
    Value v = Qnil, last = Qnil;
    for (;;) {
        Value expr = parse_expr(p);
        if (expr == Qundef)
            break;
        last = append_at(last, expr);
        if (v == Qnil)
            v = last;
    }
    free(p);
    return v;
}

Value parse(const char *path)
{
    FILE *in = fopen(path, "r");
    if (in == NULL)
        error("load: can't open file: %s", path);
    Value retval = iparse(in);
    fclose(in);
    return retval;
}

Value parse_expr_string(const char *in)
{
    if (setjmp(jmp_parse_error) != 0)
        return Qundef;
    FILE *f = fmemopen((char *) in, strlen(in), "r");
    Parser *p = parser_new(f);
    Value v = parse_expr(p);
    free(p);
    fclose(f);
    return v;
}

Value parse_string(const char *in)
{
    FILE *f = fmemopen((char *) in, strlen(in), "r");
    Value v = iparse(f);
    fclose(f);
    return v;
}

//
// Evaluation
//

static Value eval_body(Value *env, Value body)
{
    Value last = Qnil;
    for (Value b = body; b != Qnil; b = cdr(b))
        last = ieval(env, car(b));
    return last;
}

static Value map_eval(Value *env, Value l)
{
    Value mapped = Qnil, last = Qnil;
    for (; l != Qnil; l = cdr(l)) {
        last = append_at(last, ieval(env, car(l)));
        if (mapped == Qnil)
            mapped = last;
    }
    return mapped;
}

static Value eval_apply(Value *env, Value symfunc, Value args)
{
    Value func = ieval(env, symfunc);
    if (!value_tag_is(func, TAG_SPECIAL))
        args = map_eval(env, args);
    return apply(env, func, args);
}

static Value ieval(Value *env, Value v)
{
    if (value_is_symbol(v))
        return lookup(*env, v);
    if (v == Qnil || value_is_atom(v))
        return v;
    return eval_apply(env, car(v), cdr(v));
}

static Value eval_top(Value v)
{
    INIT_STACK();
    if (setjmp(jmp_runtime_error) != 0)
        return Qundef;
    return eval_body(&toplevel_environment, v);
}

Value eval(Value v)
{
    return eval_top(list(v));
}

static Value iload(FILE *in)
{
    Value l = iparse(in);
    if (l == Qundef)
        return Qundef;
    return eval_top(l);
}

static Value iload_inner(FILE *in)
{
    Value l = iparse(in);
    if (l == Qundef)
        return Qundef;
    return eval_body(&toplevel_environment, l);
}

Value eval_string(const char *in)
{
    FILE *f = fmemopen((char *) in, strlen(in), "r");
    Value v = iload(f);
    fclose(f);
    return v;
}

static FILE *open_loadable(const char *path)
{
    static char rpath[PATH_MAX];
    char joined[PATH_MAX];
    snprintf(joined, sizeof(joined), "%s/%s", load_basedir, path);
    realpath(joined, rpath);

    FILE *in = fopen(rpath, "r");
    if (in == NULL)
        error("load: can't open file: %s", path);
    load_basedir = dirname(rpath);
    return in;
}

// Current spec: path is always relative
Value load(const char *path)
{
    const char *basedir_saved = load_basedir;
    FILE *in = open_loadable(path);
    Value retval = iload(in);
    fclose(in);
    load_basedir = basedir_saved;
    return retval;
}

static Value load_inner(const char *path)
{
    const char *basedir_saved = load_basedir;
    FILE *in = open_loadable(path);
    Value retval = iload_inner(in);
    fclose(in);
    load_basedir = basedir_saved;
    return retval;
}

//
// Special Forms
//

#define UNUSED ATTR(unused)

static Value builtin_if(Value *env, Value args)
{
    expect_arity_range("if", 2, 3, args);

    Value cond = car(args), then = cadr(args);
    if (ieval(env, cond) != Qfalse)
        return ieval(env, then);
    Value els = cddr(args);
    if (els == Qnil)
        return Qnil;
    return ieval(env, car(els));
}

static Value define_variable(Value *env, Value ident, Value expr)
{
    expect_type("define", TYPE_SYMBOL, ident);

    Value val = ieval(env, expr), found;
    if (env == &toplevel_environment &&
        (found = assq(ident, *env)) != Qfalse) {
        PAIR(found)->cdr = val; // set!
    } else
        env_put(env, ident, val); // prepend new
    return Qnil;
}

static Value lambda(Value *env, Value params, Value body)
{
    expect_type_or("lambda", TYPE_PAIR, TYPE_SYMBOL, params);
    expect_type("lambda", TYPE_PAIR, body);
    if (body == Qnil)
        runtime_error("lambda: one or more expressions needed in body");
    return value_of_closure(*env, params, body);
}

static Value define_func_internal(Value *env, Value heads, Value body)
{
    Value ident = car(heads), params = cdr(heads);
    Value val = lambda(env, params, body);
    return define_variable(env, ident, val);
}

static Value builtin_define(Value *env, Value args)
{
    if (args == Qnil)
        runtime_error("define: wrong number of arguments: expected 1+");
    Value head = car(args);
    Type t = value_type_of(head);
    switch (t) {
    case TYPE_SYMBOL:
        expect_arity(2, args);
        return define_variable(env, head, cadr(args));
    case TYPE_PAIR:
        return define_func_internal(env, head, cdr(args));
    default:
        runtime_error("define: expected first argument symbol or pair but got %s",
                      value_type_to_string(t));
    }
}

static Value builtin_set(Value *env, Value ident, Value expr)
{
    expect_type("set!", TYPE_SYMBOL, ident);

    Value found = assq(ident, *env);
    if (found == Qfalse)
        runtime_error("set!: unbound variable: %s", value_to_string(ident));
    PAIR(found)->cdr = ieval(env, expr);
    return Qnil;
}

static Value builtin_let(Value *env, Value args)
{
    Value bindings = car(args);
    Value body = cdr(args);
    expect_type_twin("let", TYPE_PAIR, bindings, body);

    Value letenv = *env;
    for (; bindings != Qnil; bindings = cdr(bindings)) {
        Value b = car(bindings);
        if (b == Qnil)
            continue;
        expect_type("let", TYPE_PAIR, b);
        Value ident = car(b), expr = cadr(b);
        expect_type("let", TYPE_SYMBOL, ident);
        env_put(&letenv, ident, ieval(env, expr));
    }
    if (body == Qnil)
        runtime_error("let: one or more expressions needed in body");
    return eval_body(&letenv, body);
}

static Value builtin_letrec(Value *env, Value args)
{
    Value bindings = car(args);
    Value body = cdr(args);
    expect_type_twin("letrec", TYPE_PAIR, bindings, body);

    Value letenv = *env;
    for (Value b = bindings; b != Qnil; b = cdr(b)) {
        Value p = car(b);
        expect_type("letrec", TYPE_PAIR, p);
        Value ident = car(p);
        expect_type("letrec", TYPE_SYMBOL, ident);
        Value val = ieval(&letenv, cadr(p));
        env_put(&letenv, ident, val);
    }
    if (body == Qnil)
        runtime_error("letrec: one or more expressions needed in body");
    return eval_body(&letenv, body);
}

static Value builtin_quote(UNUSED Value *env, Value datum)
{
    return datum;
}

static inline void expect_nonnull(const char *msg, Value l)
{
    if (l == Qnil)
        runtime_error("%s: expected non-null?: %s", msg, stringify(l));
}

static Value qq_list(Value *env, Value datum, int64_t depth);

static Value qq(Value *env, Value datum, int64_t depth)
{
    if (depth == 0)
        return ieval(env, datum);
    if (datum == Qnil || value_is_atom(datum))
        return datum;
    Value a = car(datum), d = cdr(datum);
    if (a == SYM_QUASIQUOTE) {
        expect_nonnull("nested quasiquote", d);
        Value v = qq(env, car(d), depth + 1);
        return list2(a, v);
    }
    if (a == SYM_UNQUOTE || a == SYM_UNQUOTE_SPLICING) {
        expect_nonnull("unquotes in quasiquote", d);
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

static Value splice_at(Value last, Value to_splice)
{
    if (to_splice == Qnil)
        return last; // as is
    expect_type("unquote-splicing", TYPE_PAIR, to_splice);
    if (last == Qnil)
        return to_splice;
    PAIR(last)->cdr = to_splice;
    return last_pair(to_splice);
}

static Value qq_list(Value *env, Value datum, int64_t depth)
{
    Value ret = Qnil, last = Qnil;
    for (Value o = datum; o != Qnil; o = cdr(o)) {
        bool is_atom = value_is_atom(o);
        if (is_atom || is_quoted_terminal(o)) {
            expect_nonnull("quasiquote", ret);
            PAIR(last)->cdr = is_atom ? o : qq(env, o, depth);
            break;
        }
        Value elem = car(o);
        bool spliced = (value_is_pair(elem) && car(elem) == SYM_UNQUOTE_SPLICING);
        Value v = qq(env, elem, depth);
        last = spliced ? splice_at(last, v) : append_at(last, v);
        if (ret == Qnil)
            ret = last;
    }
    return ret;
}

static Value builtin_quasiquote(Value *env, Value datum)
{
    return qq(env, datum, 1);
}

static Value builtin_begin(Value *env, Value body)
{
    return eval_body(env, body);
}

static Value builtin_cond(Value *env, Value clauses)
{
    expect_arity_range("cond", 1, -1, clauses);

    for (; clauses != Qnil; clauses = cdr(clauses)) {
        Value clause = car(clauses);
        expect_type("cond", TYPE_PAIR, clause);
        Value test = car(clause);
        Value exprs = cdr(clause);
        if (test == SYM_ELSE)
            return exprs == Qnil ? Qtrue : eval_body(env, exprs);
        Value t = ieval(env, test);
        if (t != Qfalse)
            return exprs == Qnil ? t : eval_body(env, exprs);
    }
    return Qnil;
}

static Value builtin_lambda(Value *env, Value args)
{
    return lambda(env, car(args), cdr(args));
}

static Value value_of_continuation(void)
{
    Continuation *c = obj_new(sizeof(Continuation), TAG_CONTINUATION);
    c->func.arity = 1; // by spec
    return (Value) c;
}

static bool continuation_set(Value c)
{
    GET_SP(sp); // must be the first!
    Continuation *cont = CONTINUATION(c);
    cont->sp = sp;
    cont->shelter_len = stack_base - sp;
    cont->shelter = xmalloc(cont->shelter_len);
    memcpy(cont->shelter, (void *) sp, cont->shelter_len);
    return setjmp(cont->state);
}

static Value builtin_callcc(Value *env, Value f)
{
    Value cl = ieval(env, f);
    expect_type("call/cc", TYPE_CLOSURE, cl);
    Value c = value_of_continuation();
    if (continuation_set(c) != 0)
        return CONTINUATION(c)->retval;
    return apply_closure(env, cl, list1(c));
}

//
// Built-in Functions: Arithmetic
//

static int64_t value_get_int(const char *header, Value v)
{
    expect_type(header, TYPE_INT, v);
    return value_to_int(v);
}

static Value builtin_add(UNUSED Value *env, Value args)
{
    int64_t y = 0;
    for (Value l = args; l != Qnil; l = cdr(l))
        y += value_get_int("+", car(l));
    return value_of_int(y);
}

static Value builtin_sub(UNUSED Value *env, Value args)
{
    expect_arity_range("-", 1, -1, args);

    Value rest = cdr(args);
    int64_t y = 0;
    if (rest == Qnil)
        rest = args;
    else {
        y = value_get_int("-", car(args));
    }
    for (Value l = rest; l != Qnil; l = cdr(l))
        y -= value_get_int("-", car(l));
    return value_of_int(y);
}

static Value builtin_mul(UNUSED Value *env, Value args)
{
    int64_t y = 1;
    for (Value l = args; l != Qnil; l = cdr(l))
        y *= value_get_int("*", car(l));
    return value_of_int(y);
}

static Value builtin_div(UNUSED Value *env, Value args)
{
    expect_arity_range("/", 1, -1, args);

    Value rest = cdr(args);
    int64_t y = 1;
    if (rest == Qnil)
        rest = args;
    else
        y = value_get_int("/", car(args));
    for (Value l = rest; l != Qnil; l = cdr(l)) {
        int64_t x = value_get_int("/", car(l));
        if (x == 0)
            runtime_error("/: divided by zero");
        y /= x;
    }
    return value_of_int(y);
}

static Value builtin_numeq(UNUSED Value *env, Value args)
{
    expect_arity_range("=", 2, -1, args);

    int64_t x = value_get_int("=", car(args));
    while ((args = cdr(args)) != Qnil) {
        int64_t y = value_get_int("=", car(args));
        if (x != y)
            return Qfalse;
    }
    return Qtrue;
}

static Value builtin_lt(UNUSED Value *env, Value args)
{
    expect_arity_range("<", 2, -1, args);

    int64_t x = value_get_int("<", car(args));
    while ((args = cdr(args)) != Qnil) {
        int64_t y = value_get_int("<", car(args));
        if (x >= y)
            return Qfalse;
        x = y;
    }
    return Qtrue;
}

static Value builtin_gt(UNUSED Value *env, Value args)
{
    expect_arity_range(">", 2, -1, args);

    int64_t x = value_get_int(">", car(args));
    while ((args = cdr(args)) != Qnil) {
        int64_t y = value_get_int(">", car(args));
        if (x <= y)
            return Qfalse;
        x = y;
    }
    return Qtrue;
}

static Value builtin_le(UNUSED Value *env, Value args)
{
    expect_arity_range("<=", 2, -1, args);

    int64_t x = value_get_int("<=", car(args));
    while ((args = cdr(args)) != Qnil) {
        int64_t y = value_get_int("<=", car(args));
        if (x > y)
            return Qfalse;
        x = y;
    }
    return Qtrue;
}

static Value builtin_ge(UNUSED Value *env, Value args)
{
    expect_arity_range(">=", 2, -1, args);

    int64_t x = value_get_int(">=", car(args));
    while ((args = cdr(args)) != Qnil) {
        int64_t y = value_get_int(">=", car(args));
        if (x < y)
            return Qfalse;
        x = y;
    }
    return Qtrue;
}

static Value builtin_modulo(UNUSED Value *env, Value x, Value y)
{
    int64_t b = value_get_int("modulo", y);
    if (b == 0)
        runtime_error("modulo: divided by zero");
    int64_t a = value_get_int("modulo", x);
    int64_t c = a % b;
    if ((a < 0 && b > 0) || (a > 0 && b < 0))
        c += b;
    return value_of_int(c);
}

static Value builtin_not(UNUSED Value *env, Value x)
{
    return OF_BOOL(x == Qfalse);
}

//
// Built-in Functions: Lists and others
//

static Value builtin_list(UNUSED Value *env, Value args)
{
    return args;
}

static Value builtin_length(UNUSED Value *env, Value list)
{
    expect_type("length", TYPE_PAIR, list);
    return value_of_int(length(list));
}

static Value builtin_null(UNUSED Value *env, Value list)
{
    return OF_BOOL(list == Qnil);
}

static Value builtin_reverse(UNUSED Value *env, Value list)
{
    expect_type("reverse", TYPE_PAIR, list);
    return reverse(list);
}

static Value dup_list(Value l, Value *plast)
{
    Value dup = Qnil, last = Qnil;
    for (Value p = l; p != Qnil; p = cdr(p)) {
        expect_type("append", TYPE_PAIR, p);
        last = append_at(last, car(p));
        if (dup == Qnil)
            dup = last;
    }
    *plast = last;
    return dup;
}

static Value builtin_append(UNUSED Value *env, Value args)
{
    Value l = Qnil, last = Qnil;
    Value a, next;
    for (a = args; a != Qnil; a = next) {
        if ((next = cdr(a)) == Qnil)
            break;
        Value dup = dup_list(car(a), &last);
        l = append2(l, dup);
    }
    if (a != Qnil) {
        if (l == Qnil)
            l = car(a);
        else
            PAIR(last)->cdr = car(a);
    }
    return l;
}

static Value builtin_display(UNUSED Value *env, Value obj)
{
    display(obj);
    return obj;
}

static Value builtin_newline(void)
{
    puts("");
    return Qnil;
}

static Value builtin_print(UNUSED Value *env, Value obj)
{
    display(obj);
    puts("");
    return obj;
}

static Value builtin_car(UNUSED Value *env, Value pair)
{
    expect_type("car", TYPE_PAIR, pair);
    return car(pair);
}

static Value builtin_cdr(UNUSED Value *env, Value pair)
{
    expect_type("cdr", TYPE_PAIR, pair);
    return cdr(pair);
}

static Value builtin_cons(UNUSED Value *env, Value car, Value cdr)
{
    return cons(car, cdr);
}

static Value builtin_eq(UNUSED Value *env, Value x, Value y)
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
        if (x == Qnil || y == Qnil)
            return false;
        return equal(car(x), car(y)) &&
               equal(cdr(x), cdr(y));
    case TYPE_STR:
        return (strcmp(STRING(x)->body, STRING(y)->body) == 0);
    default:
        return false;
    }
}

static Value builtin_equal(UNUSED Value *env, Value x, Value y)
{
    return OF_BOOL(equal(x, y));
}


static Value builtin_assq(UNUSED Value *env, Value obj, Value alist)
{
    expect_type("assq", TYPE_PAIR, alist);
    return assq(obj, alist);
}

static Value builtin_load(UNUSED Value *env, Value path)
{
    return load_inner(value_to_string(path));
}

static void expect_proc(const char *header, Value v)
{
    Type t = value_type_of(v);
    switch (t) {
    case TYPE_CFUNC:
    case TYPE_CLOSURE:
    case TYPE_CONTINUATION:
        return;
    default:
        runtime_error("type error in %s: expected procedure but got %s",
                      header, value_type_to_string(t));
    }
}

static Value apply_args(Value args)
{
    Value heads = Qnil, last = Qnil, a, next;
    for (a = args; (next = cdr(a)) != Qnil; a = next) {
        last = append_at(last, car(a));
        if (heads == Qnil)
            heads = last;
    }
    Value rest = car(a);
    expect_type("args on apply", TYPE_PAIR, rest);
    return append2(heads, rest);
}

static Value builtin_apply(Value *env, Value args)
{
    expect_arity_range("apply", 2, -1, args);

    Value proc = car(args);
    expect_proc("apply", proc);
    Value appargs = apply_args(cdr(args));
    return apply(env, proc, appargs);
}

static bool is_procedure(Value o)
{
    return is_procedure_type(value_type_of(o));
}

static Value builtin_procedure_p(UNUSED Value *env, Value o)
{
    return OF_BOOL(is_procedure(o));
}

static bool cars_cdrs(Value ls, Value *pcars, Value *pcdrs)
{
    Value lcars = Qnil, lcdrs = Qnil;
    Value cars = Qnil , cdrs = Qnil;
    for (Value p = ls, l; p != Qnil; p = cdr(p)) {
        if ((l = car(p)) == Qnil)
            return false;

        lcars = append_at(lcars, car(l));
        if (cars == Qnil)
            cars = lcars;
        lcdrs = append_at(lcdrs, cdr(l));
        if (cdrs == Qnil)
            cdrs = lcdrs;
    }
    *pcars = cars;
    *pcdrs = cdrs;
    return true;
}

static Value builtin_map(Value *env, Value args)
{
    expect_arity_range("map", 2, -1, args);

    Value proc = car(args);
    Value lists = cdr(args);
    Value last = Qnil, ret = Qnil;
    Value cars, cdrs;
    while (cars_cdrs(lists, &cars, &cdrs)) {
        Value v = apply(env, proc, cars);
        last = append_at(last, v);
        if (ret == Qnil)
            ret = last;
        lists = cdrs;
    }
    return ret;
}

static Value builtin_for_each(Value *env, Value args)
{
    expect_arity_range("for-each", 2, -1, args);

    Value proc = car(args);
    Value lists = cdr(args);
    Value cars, cdrs;
    while (cars_cdrs(lists, &cars, &cdrs)) {
        apply(env, proc, cars);
        lists = cdrs;
    }
    return Qnil;
}

static Value builtin_unquote(UNUSED Value *env, UNUSED Value args)
{
    runtime_error("unquote: applied out of quasiquote (`)");
}

static Value builtin_unquote_splicing(UNUSED Value *env, UNUSED Value args)
{
    runtime_error("unquote-splicing: applied out of quasiquote (`)");
}

//
// Built-in Functions: Extensions
//

static Value builtin_cputime(void) // in micro sec
{
    static const int64_t MICRO = 1000*1000;
    struct timespec t;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t);
    int64_t n = t.tv_sec * MICRO + lround(t.tv_nsec / 1000.0);
    return value_of_int(n);
}

ATTR(constructor)
static void initialize(void)
{
    static char basedir[PATH_MAX];
    load_basedir = getcwd(basedir, sizeof(basedir));
    SYM_ELSE = value_of_symbol("else");
    SYM_QUOTE = value_of_symbol("quote");
    SYM_QUASIQUOTE = value_of_symbol("quasiquote");
    SYM_UNQUOTE = value_of_symbol("unquote");
    SYM_UNQUOTE_SPLICING = value_of_symbol("unquote-splicing");

    Value *e = &toplevel_environment;
    define_special(e, "if", builtin_if, -1);
    define_special(e, "define", builtin_define, -1);
    define_special(e, "set!", builtin_set, 2);
    define_special(e, "let", builtin_let, -1);
    define_special(e, "let*", builtin_let, -1); // alias
    define_special(e, "letrec", builtin_letrec, -1);
    define_special(e, "quote", builtin_quote, 1);
    define_special(e, "quasiquote", builtin_quasiquote, 1);
    define_special(e, "unquote", builtin_unquote, 1);
    define_special(e, "unquote-splicing", builtin_unquote_splicing, 1);
    define_special(e, "begin", builtin_begin, -1);
    define_special(e, "cond", builtin_cond, -1);
    define_special(e, "lambda", builtin_lambda, -1);
    define_special(e, "call/cc", builtin_callcc, 1);
    define_special(e, "call-with-current-continuation", builtin_callcc, 1); // alias

    define_function(e, "+", builtin_add, -1);
    define_function(e, "-", builtin_sub, -1);
    define_function(e, "*", builtin_mul, -1);
    define_function(e, "/", builtin_div, -1);
    define_function(e, "=", builtin_numeq, -1);
    define_function(e, "<", builtin_lt, -1);
    define_function(e, ">", builtin_gt, -1);
    define_function(e, "<=", builtin_le, -1);
    define_function(e, ">=", builtin_ge, -1);
    define_function(e, "modulo", builtin_modulo, 2);
    define_function(e, "not", builtin_not, 1);

    define_function(e, "car", builtin_car, 1);
    define_function(e, "cdr", builtin_cdr, 1);
    define_function(e, "cons", builtin_cons, 2);
    define_function(e, "list", builtin_list, -1);
    define_function(e, "length", builtin_length, 1);
    define_function(e, "null?", builtin_null, 1);
    define_function(e, "reverse", builtin_reverse, 1);
    define_function(e, "append", builtin_append, -1);
    define_function(e, "display", builtin_display, 1);
    define_function(e, "newline", builtin_newline, 0);
    define_function(e, "print", builtin_print, 1);
    define_function(e, "eq?", builtin_eq, 2);
    define_function(e, "equal?", builtin_equal, 2);
    define_function(e, "assq", builtin_assq, 2);
    define_function(e, "load", builtin_load, 1);
    define_function(e, "apply", builtin_apply, -1);
    define_function(e, "procedure?", builtin_procedure_p, 1);
    define_function(e, "map", builtin_map, -1);
    define_function(e, "for-each", builtin_for_each, -1);

    define_function(e, "_cputime", builtin_cputime, 0);
}
