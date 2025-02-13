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

#include "schaf.h"
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
    [TYPE_PROC] = "procedure",
};

typedef enum { // has the same values as Type
    TAG_PAIR    = TYPE_PAIR,
    TAG_STR     = TYPE_STR,
    TAG_CFUNC   = TYPE_PROC + 1,
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

#define VALUE_TAG(v) (*(ValueTag*)(v))
#define PAIR(v) ((Pair *) v)
#define STRING(v) ((String *) v)
#define PROCEDURE(v) ((Procedure *) v)
#define CFUNC(v) ((CFunc *) v)
#define CLOSURE(v) ((Closure *) v)
#define CONTINUATION(v) ((Continuation *) v)
#define OF_BOOL(v) ((v) ? Qtrue : Qfalse)

// singletons
static const Pair PAIR_NIL = { .tag = TAG_PAIR, .car = 0, .cdr = 0 };
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
const Value Qnil = (Value) &PAIR_NIL;
const Value Qfalse = 0b00100U;
const Value Qtrue  = 0b01100U;
const Value Qundef = 0b10100U; // may be an error or something

static const int64_t CFUNCARG_MAX = 7;

//
// Runtime-locals (aka global variables)
//

static Value toplevel_environment = Qnil; // alist of ('symbol . <value>)
static Value symbol_names = Qnil; // ("name0" "name1" ...)
static Value SYM_ELSE, SYM_QUOTE, SYM_QUASIQUOTE, SYM_UNQUOTE, SYM_UNQUOTE_SPLICING,
    SYM_RARROW;
static const volatile void *stack_base = NULL;
#define INIT_STACK() void *basis; stack_base = &basis
static const char *load_basedir = NULL;
static Value call_stack = Qnil;
static Value source_data = Qnil;

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

static inline bool is_immediate(Value v)
{
    return v & FLAG_MASK;
}

static inline bool value_tag_is(Value v, ValueTag expected)
{
    return !is_immediate(v) && VALUE_TAG(v) == expected;
}

inline bool value_is_string(Value v)
{
    return value_tag_is(v, TAG_STR);
}

static inline bool value_is_procedure(Value v)
{
    if (is_immediate(v))
        return false;
    switch (VALUE_TAG(v)) {
    case TAG_SYNTAX:
    case TAG_CFUNC:
    case TAG_CLOSURE:
    case TAG_CONTINUATION:
        return true;
    default:
        return false;
    }
}

inline bool value_is_pair(Value v)
{
    return value_tag_is(v, TAG_PAIR);
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
        return (Type) t;
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

static inline Value list1(Value x)
{
    return cons(x, Qnil);
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

static void *obj_new(size_t size, ValueTag t)
{
    void *p = xmalloc(size);
    VALUE_TAG(p) = t;
    return p;
}

Value value_of_string(const char *s)
{
    size_t len = strlen(s) + 1;
    String *str = obj_new(sizeof(String) + len, TAG_STR);
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

static Value value_of_cfunc(cfunc_t cfunc, int64_t arity)
{
    expect_cfunc_arity(arity);
    CFunc *f = obj_new(sizeof(CFunc), TAG_CFUNC);
    f->proc.arity = arity;
    f->cfunc = cfunc;
    return (Value) f;
}

static Value value_of_syntax(cfunc_t cfunc, int64_t arity)
{
    Value sp = value_of_cfunc(cfunc, arity);
    VALUE_TAG(sp) = TAG_SYNTAX;
    return sp;
}

static Value value_of_closure(Value env, Value params, Value body)
{
    Closure *f = obj_new(sizeof(Closure), TAG_CLOSURE);
    f->proc.arity = value_is_pair(params) ? length(params) : -1;
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
    const char *filename;
    Token prev_token;
    Value function_locations; //  alist of '(id . (pos . sym)) | id = (pointer >> 3)
    Value newline_pos; // list of pos | int
} Parser;

static void pos_to_line_col(int64_t pos, Value newline_pos, int64_t *line, int64_t *col)
{
    int64_t nline = 0, last = 0;
    for (Value p = newline_pos; p != Qnil; p = cdr(p), nline++) {
        int n = value_to_int(car(p));
        if (n > pos)
            break;
        last = n;
    }
    *line = nline + 1;
    *col = pos - last + 1;
}

static Value reverse(Value l);

ATTR(noreturn)
static void parse_error(Parser *p, const char *expected, const char *actual, ...)
{
    Value newline_pos = reverse(p->newline_pos);
    int64_t pos = ftell(p->in);
    int64_t line, col;
    pos_to_line_col(pos, newline_pos, &line, &col);
    int n = snprintf(errmsg, sizeof(errmsg),
                     "%s:%"PRId64":%"PRId64": expected %s but got ",
                     p->filename, line, col, expected);
    va_list ap;
    va_start(ap, actual);
    vsnprintf(errmsg + n, sizeof(errmsg) - n, actual, ap);
    va_end(ap);
    longjmp(jmp_parse_error, 1);
}

static inline void put_newline_pos(Parser *p)
{
    p->newline_pos = cons(value_of_int(ftell(p->in)), p->newline_pos);
}

static void skip_token_atmosphere(Parser *p)
{
    int c;
    for (;;) {
        c = fgetc(p->in);
        if (isspace(c)) {
            if (c == '\n')
                put_newline_pos(p);
            continue; // skip
        }
        if (c == ';') {
            do {
                c = fgetc(p->in);
            } while (c != '\n' && c != EOF);
            if (c == '\n')
                put_newline_pos(p);
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

static Token get_token_dots(Parser *p)
{
    int c = fgetc(p->in);
    if (c != '.') {
        ungetc(c, p->in);
        return TOK_DOT;
    }
    c = fgetc(p->in);
    if (c != '.') {
        ungetc(c, p->in);
        return TOK_IDENT("..");
    }
    return TOK_IDENT("...");
}

static Token get_token_string(Parser *p)
{
    char buf[BUFSIZ], *pbuf = buf, *end = pbuf + sizeof(buf) - 2;
    for (int c; (c = fgetc(p->in)) != '"'; *pbuf++ = c) {
        if (c == '\\') {
            c = fgetc(p->in);
            if (c != '\\' && c != '"')
                parse_error(p, "'\\' or '\"' in string literal", "'%c'", c);
        }
        if (pbuf == end)
            parse_error(p, "string literal", "too long: \"%s...\"", pbuf);
    }
    *pbuf = '\0';
    return TOK_STR(buf);
}

static Token get_token_constant(Parser *p)
{
    int c = fgetc(p->in);
    switch (c) {
    case 't':
        return TOK_CONST(Qtrue);
    case 'f':
        return TOK_CONST(Qfalse);
    default:
        parse_error(p, "constants", "#%c", c);
    }
}

static Token get_token_int(Parser *p, int c, int sign)
{
    ungetc(c, p->in);
    int64_t i;
    int n = fscanf(p->in, "%"SCNd64, &i);
    if (n != 1)
        parse_error(p, "integer", "invalid string");
    return TOK_INT(sign * i);
}

static Token get_token_after_sign(Parser *p, int csign)
{
    int c = fgetc(p->in);
    int dig = isdigit(c);
    if (dig) {
        int sign = csign == '-' ? -1 : 1;
        return get_token_int(p, c, sign);
    }
    ungetc(c, p->in);
    return TOK_IDENT(((char []) { csign, '\0' }));
}

static inline bool is_special_initial(int c)
{
    switch (c) {
    case '!': case '$': case '%': case '&': case '*': case '/': case ':':
    case '<': case '=': case '>': case '?': case '^': case '_': case '~':
        return true;
    default:
        return false;
    }
}

static inline bool is_initial(int c)
{
    return isalpha(c) || is_special_initial(c);
}

static inline bool is_special_subsequent(int c)
{
    return c == '+' || c == '-' || c == '.' || c == '@';
}

static inline bool is_subsequent(int c)
{
    return is_initial(c) || isdigit(c) || is_special_subsequent(c);
}

static Token get_token_ident(Parser *p, int init)
{
    char buf[BUFSIZ], *s = buf, *end = s + sizeof(buf);
    int c;
    for (*s++ = init; is_subsequent(c = fgetc(p->in)); *s++ = c) {
        if (s == end)
            parse_error(p, "identifier", "too long");
    }
    ungetc(c, p->in);
    *s = '\0';
    return TOK_IDENT(buf);
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
        return get_token_dots(p);
    case '"':
        return get_token_string(p);
    case '#':
        return get_token_constant(p);
    case '+':
    case '-':
        return get_token_after_sign(p, c);
    case EOF:
        return TOK_EOF;
    default:
        break;
    }
    if (isdigit(c))
        return get_token_int(p, c, 1);
    if (is_initial(c))
        return get_token_ident(p, c);
    parse_error(p, "valid char", "'%c'", c);
}

static void unget_token(Parser *p, Token t)
{
    p->prev_token = t;
}

#define CXR1(f, x) f(a, x); f(d, x);
#define CXR2(f, x) CXR1(f, a ## x) CXR1(f, d ## x)
#define CXR3(f, x) CXR2(f, a ## x) CXR2(f, d ## x)
#define CXR4(f, x) CXR3(f, a) CXR3(f, d)
#define CXRS(f) CXR2(f,) CXR3(f,) CXR4(f,)

#define DEF_CXR(x, y) \
    static Value c##x##y##r(Value v) { return c##x##r(c##y##r(v)); }
CXRS(DEF_CXR)

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

static Value parse_expr(Parser *p);

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

static Value append_at(Value last, Value elem)
{
    Value p = list1(elem);
    if (last != Qnil)
        PAIR(last)->cdr = p;
    return p;
}

static inline Value pair_to_id(Value p)
{
    return value_of_int(p >> 3U); // we assume 64 bit machines
}

static void record_location(Parser *p, Value pair, int64_t pos, Value sym)
{
    int64_t id = pair_to_id(pair);
    Value loc = cons(id, cons(value_of_int(pos), sym));
    p->function_locations = cons(loc, p->function_locations);
}

static Value parse_list(Parser *p)
{
    Value l = Qnil, last = Qnil;
    int64_t pos = ftell(p->in);
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
        if (l == Qnil) {
            l = last;
            if (value_is_symbol(e))
                record_location(p, l, pos, e);
        }
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

static Parser *parser_new(FILE *in, const char *filename)
{
    Parser *p = xmalloc(sizeof(Parser));
    p->in = in;
    p->filename = filename;
    p->prev_token = TOK_EOF; // we use this since we never postpone EOF things
    p->function_locations = Qnil;
    p->newline_pos = Qnil;
    return p;
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

static Value apply_cfunc(Value *env, Value proc, Value args)
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

static inline void env_put(Value *env, Value name, Value val)
{
    *env = cons(cons(name, val), *env);
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

static Value eval_body(Value *env, Value body);

//PTR
static Value apply_closure(Value *env, Value proc, Value args)
{
    Closure *cl = CLOSURE(proc);
    int64_t arity = cl->proc.arity;
    Value clenv = append2(cl->env, *env), params = cl->params;
    if (arity == -1)
        env_put(&clenv, params, args);
    else {
        for (Value p = args; p != Qnil; p = cdr(p), params = cdr(params))
            env_put(&clenv, car(params), car(p));
    }
    return eval_body(&clenv, cl->body);
}

static inline void expect_nonnull(const char *msg, Value l)
{
    expect_type(msg, TYPE_PAIR, l);
    if (l == Qnil)
        runtime_error("%s: expected non-null?", msg);
}

static void call_stack_push(Value l)
{
    call_stack = cons(pair_to_id(l), call_stack);
}

static void call_stack_pop(void)
{
    expect_nonnull("apply", call_stack);
    call_stack = cdr(call_stack);
}

ATTR(noreturn) ATTR(noinline)
static void jump(Continuation *cont)
{
    call_stack = cont->call_stack;
    memcpy((void *) cont->sp, cont->shelter, cont->shelter_len);
    longjmp(cont->state, 1);
}

#define GET_SP(p) volatile void *p = &p

ATTR(noreturn) ATTR(noinline)
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

// expects proc and args are evaluated if necessary
static Value apply(Value *env, Value proc, Value args)
{
    expect_arity(PROCEDURE(proc)->arity, args);
    switch (VALUE_TAG(proc)) {
    case TAG_SYNTAX:
    case TAG_CFUNC:
        return apply_cfunc(env, proc, args);
    case TAG_CLOSURE:
        return apply_closure(env, proc, args);
    case TAG_CONTINUATION:
        apply_continuation(proc, args); // no return!
    default:
        UNREACHABLE();
    }
}

// Note: Do not mistake this for "(define-syntax ...)" which related to macros
static void define_syntax(Value *env, const char *name, cfunc_t cfunc, int64_t arity)
{
    env_put(env, value_of_symbol(name), value_of_syntax(cfunc, arity));
}

static void define_procedure(Value *env, const char *name, cfunc_t cfunc, int64_t arity)
{
    env_put(env, value_of_symbol(name), value_of_cfunc(cfunc, arity));
}

static Value assq(Value key, Value l);

static Value lookup(Value env, Value name)
{
    Value found = assq(name, env);
    if (found == Qfalse)
        runtime_error("unbound variable: %s", value_to_string(name));
    return cdr(found);
}

static inline Value list4(Value w, Value x, Value y, Value z)
{
    return cons(w, cons(x, list2(y, z)));
}

// AST: (syntax_list filename function_locations newline_positions)
static Value ast_new(Parser *p, Value syntax_list)
{
    Value filename = value_of_symbol(p->filename);
    return list4(syntax_list, filename, p->function_locations, reverse(p->newline_pos));
}

static Value parse_program(Parser *p)
{
    Value v = Qnil, last = Qnil;
    for (Value expr; (expr = parse_expr(p)) != Qundef; ) {
        last = append_at(last, expr);
        if (v == Qnil)
            v = last;
    }
    return ast_new(p, v);
}

static Value iparse(FILE *in, const char *filename)
{
    Parser *p = parser_new(in, filename);
    Value ast;
    if (setjmp(jmp_parse_error) == 0)
        ast = parse_program(p); // success
    else
        ast = ast_new(p, Qundef); // got an error
    free(p);
    return ast;
}

Value parse(const char *path)
{
    FILE *in = fopen(path, "r");
    if (in == NULL)
        error("parse: can't open file: %s", path);
    Value ast = iparse(in, path);
    fclose(in);
    return car(ast);
}

Value parse_string(const char *in)
{
    FILE *f = fmemopen((char *) in, strlen(in), "r");
    Value ast = iparse(f, "<inline>");
    fclose(f);
    return car(ast);
}

//
// Evaluation
//
static Value eval(Value *env, Value v);

static Value eval_body(Value *env, Value body)
{
    if (body == Qnil)
        return Qnil;
    Value p = body;
    for (Value next; (next = cdr(p)) != Qnil; p = next)
        eval(env, car(p));
    return eval(env, car(p));
}

static Value map_eval(Value *env, Value l)
{
    Value mapped = Qnil, last = Qnil;
    for (Value p = l; p != Qnil; p = cdr(p)) {
        last = append_at(last, eval(env, car(p)));
        if (mapped == Qnil)
            mapped = last;
    }
    return mapped;
}

static Value eval_apply(Value *env, Value symproc, Value args)
{
    Value proc = eval(env, symproc);
    expect_type("eval", TYPE_PROC, proc);
    if (!value_tag_is(proc, TAG_SYNTAX))
        args = map_eval(env, args);
    Value ret = apply(env, proc, args);
    call_stack_pop();
    return ret;
}

static Value eval(Value *env, Value v)
{
    if (value_is_symbol(v))
        return lookup(*env, v);
    if (v == Qnil || !value_is_pair(v))
        return v;
    call_stack_push(v);
    return eval_apply(env, car(v), cdr(v));
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

static void dump_line_column(Value vfilename, Value vpos)
{
    int64_t pos = value_to_int(vpos), line, col;
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

static Value find_location_by_pair_id(Value id, Value *pfilename)
{
    for (Value p = source_data; p != Qnil; p = cdr(p)) {
        Value filename = caar(p), locations = cadar(p);
        Value found = assq(id, locations);
        if (found != Qfalse) {
            if (pfilename)
                *pfilename = filename;
            return found;
        }
    }
    return Qfalse;
}

static void dump_callee_name(Value callers)
{
    if (callers == Qnil) {
        append_error_message("<toplevel>");
        return;
    }
    Value id = car(callers);
    Value found = find_location_by_pair_id(id, NULL);
    if (found == Qfalse)
        append_error_message("<unknown>");
    else {
        const char *name = value_to_string(cddr(found));
        append_error_message("'%s'", name);
    }
}

static void dump_frame(Value id, Value callers)
{
    Value filename, found = find_location_by_pair_id(id, &filename);
    if (found == Qfalse) {
        append_error_message("\n\t<unknown>");
        return;
    }
    dump_line_column(filename, cadr(found));
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
    Value ast = iparse(in, filename), l = car(ast);
    source_data = cons(cdr(ast), source_data);
    if (l == Qundef)
        return Qundef;
    if (setjmp(jmp_runtime_error) != 0) {
        dump_stack_trace();
        return Qundef;
    }
    INIT_STACK();
    call_stack = Qnil;
    Value ret = eval_body(&toplevel_environment, l);
    call_stack_check_consistency();
    return ret;
}

static Value iload_inner(FILE *in, const char *path)
{
    Value ast = iparse(in, path), l = car(ast);
    source_data = cons(cdr(ast), source_data);
    if (l == Qundef)
        return Qundef;
    return eval_body(&toplevel_environment, l);
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
        error("load: can't open file: %s", path);
    load_basedir = dirname(rpath);
    return in;
}

Value load(const char *path)
{
    FILE *in = open_loadable(path);
    Value retval = iload(in, path);
    fclose(in);
    return retval;
}

static Value load_inner(const char *path)
{
    const char *basedir_saved = load_basedir;
    FILE *in = open_loadable(path);
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
static Value syn_quote(UNUSED Value *env, Value datum)
{
    return datum;
}

// 4.1.4. Procedures
static Value lambda(Value *env, Value params, Value body)
{
    expect_type_or("lambda", TYPE_PAIR, TYPE_SYMBOL, params);
    expect_type("lambda", TYPE_PAIR, body);
    if (body == Qnil)
        runtime_error("lambda: one or more expressions needed in body");
    return value_of_closure(*env, params, body);
}

//PTR -- proper tail recursion needed
static Value syn_lambda(Value *env, Value args)
{
    return lambda(env, car(args), cdr(args));
}

// 4.1.5. Conditionals
//PTR
static Value syn_if(Value *env, Value args)
{
    expect_arity_range("if", 2, 3, args);

    Value cond = car(args), then = cadr(args);
    if (eval(env, cond) != Qfalse)
        return eval(env, then);
    Value els = cddr(args);
    if (els == Qnil)
        return Qnil;
    return eval(env, car(els));
}

// 4.1.6. Assignments
static Value iset(Value *env, Value ident, Value val)
{
    Value found = assq(ident, *env);
    if (found == Qfalse)
        runtime_error("set!: unbound variable: %s", value_to_string(ident));
    PAIR(found)->cdr = val;
    return Qnil;
}

static Value syn_set(Value *env, Value ident, Value expr)
{
    expect_type("set!", TYPE_SYMBOL, ident);
    return iset(env, ident, eval(env, expr));
}

// 4.2. Derived expression types
// 4.2.1. Conditionals
static inline void expect_null(const char *msg, Value l)
{
    if (l != Qnil)
        runtime_error("%s: expected null?", msg);
}

static Value cond_eval_recipient(Value *env, Value test, Value recipients)
{
    expect_nonnull("recipient in cond", recipients);
    Value recipient = eval(env, car(recipients)), rest = cdr(recipients);
    expect_type("end of => in cond", TYPE_PROC, recipient);
    expect_null("end of => in cond", rest);
    return apply(env, recipient, list1(test));
}

//PTR
static Value syn_cond(Value *env, Value clauses)
{
    expect_arity_range("cond", 1, -1, clauses);

    for (Value p = clauses; p != Qnil; p = cdr(p)) {
        Value clause = car(p);
        expect_nonnull("clause in cond", clause);
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
static Value syn_case(Value *env, Value args)
{
    expect_arity_range("case", 2, -1, args);
    Value key = eval(env, car(args)), clauses = cdr(args);

    for (Value p = clauses; p != Qnil; p = cdr(p)) {
        Value clause = car(p);
        expect_nonnull("case", clause);
        Value data = car(clause), exprs = cdr(clause);
        expect_nonnull("case", exprs);
        if (data == SYM_ELSE || memq(key, data) != Qfalse)
            return eval_body(env, exprs);
    }
    return Qnil;
}

//PTR
static Value syn_and(Value *env, Value args)
{
    if (args == Qnil)
        return Qtrue;
    Value p = args;
    for (Value next; (next = cdr(p)) != Qnil; p = next) {
        if (eval(env, car(p)) == Qfalse)
            return Qfalse;
    }
    return eval(env, car(p));
}

//PTR
static Value syn_or(Value *env, Value args)
{
    if (args == Qnil)
        return Qfalse;
    Value p = args;
    for (Value next, v; (next = cdr(p)) != Qnil; p = next) {
        if ((v = eval(env, car(p))) != Qfalse)
            return v;
    }
    return eval(env, car(p));
}

// 4.2.2. Binding constructs
static Value transpose_2xn(Value ls) // 2 * n
{
    Value firsts = Qnil, seconds = Qnil;
    Value lfirsts = Qnil, lseconds = Qnil;
    for (Value p = ls; p != Qnil; p = cdr(p)) {
        Value l = car(p);
        expect_arity(2, l);
        lfirsts = append_at(lfirsts, car(l));
        lseconds = append_at(lseconds, cadr(l));
        if (firsts == Qnil) {
            firsts = lfirsts;
            seconds = lseconds;
        }
    }
    return list2(firsts, seconds);
}

static Value define_variable(Value *env, Value ident, Value expr);

static Value let(Value *env, const char *func, Value bindings, Value body)
{
    expect_type(func, TYPE_PAIR, bindings);
    Value letenv = *env;
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        Value b = car(p);
        if (b == Qnil)
            continue;
        expect_type(func, TYPE_PAIR, b);
        Value ident = car(b), expr = cadr(b);
        expect_type(func, TYPE_SYMBOL, ident);
        env_put(&letenv, ident, eval(env, expr));
    }
    return eval_body(&letenv, body);
}

static Value named_let(Value *env, Value var, Value bindings, Value body)
{
    Value tr = transpose_2xn(bindings);
    Value params = car(tr), symargs = cadr(tr);
    Value proc = lambda(env, params, body);
    expect_type("named let", TYPE_PROC, proc);
    Value args = map_eval(env, symargs);
    Value letenv = *env;
    define_variable(&letenv, var, proc);
    return apply(&letenv, proc, args);
}

//PTR
static Value syn_let(Value *env, Value args)
{
    expect_arity_range("let", 2, -1, args);
    Value bind_or_var = car(args), body = cdr(args);
    if (value_is_symbol(bind_or_var))
        return named_let(env, bind_or_var, car(body), cdr(body));
    return let(env, "let", bind_or_var, body);
}

//PTR
static Value syn_let_star(Value *env, Value args)
{
    expect_arity_range("let*", 2, -1, args);
    return let(env, "let*", car(args), cdr(args));
}

//PTR
static Value syn_letrec(Value *env, Value args)
{
    expect_arity_range("letrec", 2, -1, args);
    Value bindings = car(args);
    Value body = cdr(args);
    expect_type_twin("letrec", TYPE_PAIR, bindings, body);
    if (body == Qnil)
        runtime_error("letrec: one or more expressions needed in body");

    Value letenv = *env;
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        Value b = car(p);
        expect_type("letrec", TYPE_PAIR, b);
        Value ident = car(b);
        expect_type("letrec", TYPE_SYMBOL, ident);
        define_variable(&letenv, ident, cadr(b));
    }
    return eval_body(&letenv, body);
}

// 4.2.3. Sequencing
//PTR
static Value syn_begin(Value *env, Value body)
{
    return eval_body(env, body);
}

// 4.2.4. Iteration
//PTR
static Value syn_do(Value *env, Value args)
{
    expect_arity_range("do", 2, -1, args);

    Value bindings = car(args), tests = cadr(args), body = cddr(args);
    expect_type_twin("do", TYPE_PAIR, bindings, tests);
    Value doenv = *env, steps = Qnil;
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        Value b = car(p);
        expect_nonnull("do", b);
        Value var = car(b), init = cadr(b), step = cddr(b);
        if (step != Qnil)
            steps = cons(cons(var, car(step)), steps);
        expect_type("do", TYPE_SYMBOL, var);
        env_put(&doenv, var, eval(env, init)); // in the original env
    }
    Value test = car(tests), exprs = cdr(tests);
    while (eval(&doenv, test) == Qfalse) {
        if (body != Qnil)
            eval_body(&doenv, body);
        for (Value p = steps; p != Qnil; p = cdr(p)) {
            Value pstep = car(p);
            Value var = car(pstep), step = cdr(pstep);
            Value val = eval(&doenv, step);
            iset(&doenv, var, val);
        }
    }
    return exprs == Qnil ? Qnil : eval_body(&doenv, exprs);
}

// 4.2.6. Quasiquotation
static Value qq_list(Value *env, Value datum, int64_t depth);

static Value qq(Value *env, Value datum, int64_t depth)
{
    if (depth == 0)
        return eval(env, datum);
    if (datum == Qnil || !value_is_pair(datum))
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
    for (Value p = datum; p != Qnil; p = cdr(p)) {
        bool is_simple = !value_is_pair(p);
        if (is_simple || is_quoted_terminal(p)) {
            expect_nonnull("quasiquote", ret);
            PAIR(last)->cdr = is_simple ? p : qq(env, p, depth);
            break;
        }
        Value elem = car(p);
        bool spliced = (value_is_pair(elem) && car(elem) == SYM_UNQUOTE_SPLICING);
        Value v = qq(env, elem, depth);
        last = spliced ? splice_at(last, v) : append_at(last, v);
        if (ret == Qnil)
            ret = last;
    }
    return ret;
}

static Value syn_quasiquote(Value *env, Value datum)
{
    return qq(env, datum, 1);
}

static Value syn_unquote(UNUSED Value *env, UNUSED Value args)
{
    runtime_error("unquote: applied out of quasiquote (`)");
}

static Value syn_unquote_splicing(UNUSED Value *env, UNUSED Value args)
{
    runtime_error("unquote-splicing: applied out of quasiquote (`)");
}

// 5.2. Definitions
static Value define_variable(Value *env, Value ident, Value expr)
{
    expect_type("define", TYPE_SYMBOL, ident);

    Value val = eval(env, expr), found;
    if (env == &toplevel_environment &&
        (found = assq(ident, *env)) != Qfalse) {
        PAIR(found)->cdr = val; // set!
    } else
        env_put(env, ident, val); // prepend new
    return Qnil;
}

static Value define_proc_internal(Value *env, Value heads, Value body)
{
    Value ident = car(heads), params = cdr(heads);
    Value val = lambda(env, params, body);
    return define_variable(env, ident, val);
}

static Value syn_define(Value *env, Value args)
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
        return define_proc_internal(env, head, cdr(args));
    default:
        runtime_error("define: expected first argument symbol or pair but got %s",
                      value_type_to_string(t));
    }
}

// 6.1. Equivalence predicates
static inline bool eq(Value x, Value y)
{
    return x == y;
}

static Value proc_eq(UNUSED Value *env, Value x, Value y)
{
    return OF_BOOL(eq(x, y));
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

static Value proc_equal(UNUSED Value *env, Value x, Value y)
{
    return OF_BOOL(equal(x, y));
}

// 6.2.5. Numerical operations
static int64_t value_get_int(const char *header, Value v)
{
    expect_type(header, TYPE_INT, v);
    return value_to_int(v);
}

static Value proc_integer_p(UNUSED Value *env, Value obj)
{
    return OF_BOOL(value_is_int(obj));
}

static Value proc_numeq(UNUSED Value *env, Value args)
{
    expect_arity_range("=", 2, -1, args);

    int64_t x = value_get_int("=", car(args));
    for (args = cdr(args); x == value_get_int("=", car(args)); ) {
        if ((args = cdr(args)) == Qnil)
            return Qtrue;
    }
    return Qfalse;
}

static Value proc_lt(UNUSED Value *env, Value args)
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

static Value proc_gt(UNUSED Value *env, Value args)
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

static Value proc_le(UNUSED Value *env, Value args)
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

static Value proc_ge(UNUSED Value *env, Value args)
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

static Value proc_zero_p(UNUSED Value *env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && value_to_int(obj) == 0);
}

static Value proc_positive_p(UNUSED Value *env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && value_to_int(obj) > 0);
}

static Value proc_negative_p(UNUSED Value *env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && value_to_int(obj) < 0);
}

static Value proc_odd_p(UNUSED Value *env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && (value_to_int(obj) % 2) != 0);
}

static Value proc_even_p(UNUSED Value *env, Value obj)
{
    return OF_BOOL(value_is_int(obj) && (value_to_int(obj) % 2) == 0);
}

static Value proc_max(UNUSED Value *env, Value args)
{
    expect_arity_range("max", 1, -1, args);
    int64_t max = value_get_int("max", car(args));
    for (Value p = cdr(args); p != Qnil; p = cdr(p)) {
        int64_t x = value_get_int("max", car(p));
        if (max < x)
            max = x;
    }
    return value_of_int(max);
}

static Value proc_min(UNUSED Value *env, Value args)
{
    expect_arity_range("min", 1, -1, args);
    int64_t min = value_get_int("min", car(args));
    for (Value p = cdr(args); p != Qnil; p = cdr(p)) {
        int64_t x = value_get_int("min", car(p));
        if (min > x)
            min = x;
    }
    return value_of_int(min);
}

static Value proc_add(UNUSED Value *env, Value args)
{
    int64_t y = 0;
    for (Value p = args; p != Qnil; p = cdr(p))
        y += value_get_int("+", car(p));
    return value_of_int(y);
}

static Value proc_sub(UNUSED Value *env, Value args)
{
    expect_arity_range("-", 1, -1, args);

    int64_t y = value_get_int("-", car(args));
    if ((args = cdr(args)) == Qnil)
        return value_of_int(-y);
    for (Value p = args; p != Qnil; p = cdr(p))
        y -= value_get_int("-", car(p));
    return value_of_int(y);
}

static Value proc_mul(UNUSED Value *env, Value args)
{
    int64_t y = 1;
    for (Value p = args; p != Qnil; p = cdr(p))
        y *= value_get_int("*", car(p));
    return value_of_int(y);
}

static Value proc_div(UNUSED Value *env, Value args)
{
    expect_arity_range("/", 1, -1, args);

    int64_t y = value_get_int("/", car(args));
    if ((args = cdr(args)) == Qnil)
       return value_of_int(1 / y);
    for (Value p = args; p != Qnil; p = cdr(p)) {
        int64_t x = value_get_int("/", car(p));
        if (x == 0)
            runtime_error("/: divided by zero");
        y /= x;
    }
    return value_of_int(y);
}

static Value proc_abs(UNUSED Value *env, Value x)
{
    int64_t n = value_get_int("abs", x);
    return value_of_int(n < 0 ? -n : n);
}

static Value proc_quotient(UNUSED Value *env, Value x, Value y)
{
    int64_t b = value_get_int("quotient", y);
    if (b == 0)
        runtime_error("quotient: divided by zero");
    int64_t a = value_get_int("quotient", x);
    int64_t c = a / b;
    return value_of_int(c);
}


static Value proc_remainder(UNUSED Value *env, Value x, Value y)
{
    int64_t b = value_get_int("remainder", y);
    if (b == 0)
        runtime_error("remainder: divided by zero");
    int64_t a = value_get_int("remainder", x);
    int64_t c = a % b;
    return value_of_int(c);
}

static Value proc_modulo(UNUSED Value *env, Value x, Value y)
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

static Value proc_expt(UNUSED Value *env, Value x, Value y)
{
    int64_t a = value_get_int("expt", x);
    int64_t b = value_get_int("expt", y);
    if (b < 0)
        runtime_error("expt", "cannot power %d which negative", b);
    int64_t c;
    if (b == 0)
        c = 1;
    else if (a == 0)
        c = 0;
    else
        c = expt(a, b);
    return value_of_int(c);
}

// 6.3.1. Booleans
static Value proc_not(UNUSED Value *env, Value x)
{
    return OF_BOOL(x == Qfalse);
}

static Value proc_boolean_p(UNUSED Value *env, Value x)
{
    return OF_BOOL(x == Qtrue || x == Qfalse);
}

// 6.3.2. Pairs and lists
static Value proc_pair_p(UNUSED Value *env, Value o)
{
    return OF_BOOL(o != Qnil && value_is_pair(o));
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

static Value proc_cons(UNUSED Value *env, Value car, Value cdr)
{
    return cons(car, cdr);
}

static Value proc_car(UNUSED Value *env, Value pair)
{
    expect_nonnull("car", pair);
    return car(pair);
}

static Value proc_cdr(UNUSED Value *env, Value pair)
{
    expect_nonnull("cdr", pair);
    return cdr(pair);
}

static Value proc_null_p(UNUSED Value *env, Value list)
{
    return OF_BOOL(list == Qnil);
}

static Value proc_list_p(UNUSED Value *env, Value list)
{
    for (Value p = list; p != Qnil; p = cdr(p)) {
        if (!value_is_pair(p))
            return Qfalse;
    }
    return Qtrue;
}

static Value proc_list(UNUSED Value *env, Value args)
{
    return args;
}

int64_t length(Value list)
{
    int64_t len = 0;
    for (Value p = list; p != Qnil; p = cdr(p))
        len++;
    return len;
}

static Value proc_length(UNUSED Value *env, Value list)
{
    expect_type("length", TYPE_PAIR, list);
    return value_of_int(length(list));
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

static Value proc_append(UNUSED Value *env, Value args)
{
    Value l = Qnil, last = Qnil;
    Value p, next;
    for (p = args; p != Qnil; p = next) {
        if ((next = cdr(p)) == Qnil)
            break;
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

static Value reverse(Value l)
{
    Value ret = Qnil;
    for (Value p = l; p != Qnil; p = cdr(p))
        ret = cons(car(p), ret);
    return ret;
}

static Value proc_reverse(UNUSED Value *env, Value list)
{
    expect_type("reverse", TYPE_PAIR, list);
    return reverse(list);
}

static Value list_tail(const char *func, Value list, Value k)
{
    expect_type(func, TYPE_PAIR, list);
    expect_type(func, TYPE_INT, k);
    int64_t n = value_to_int(k);
    if (n < 0)
        runtime_error("%s: 2nd element needs to be non-negative: "PRId64, n);
    Value p = list;
    for (int64_t i = 0; p != Qnil; p = cdr(p), i++) {
        if (i == n)
            break;
    }
    if (p == Qnil)
        runtime_error("%s: list has fewer than "PRId64" element", func, n);
    return p;
}

static Value proc_list_tail(UNUSED Value *env, Value list, Value k)
{
    return list_tail("list-tail", list, k);
}

static Value proc_list_ref(UNUSED Value *env, Value list, Value k)
{
    return car(list_tail("list-ref", list, k));
}

static Value memq(Value key, Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        Value e = car(p);
        if (eq(e, key))
            return p;
    }
    return Qfalse;
}

static Value proc_memq(UNUSED Value *env, Value obj, Value list)
{
    expect_type("memq", TYPE_PAIR, list);
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

static Value proc_member(UNUSED Value *env, Value obj, Value list)
{
    expect_type("member", TYPE_PAIR, list);
    return member(obj, list);
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

static Value proc_assq(UNUSED Value *env, Value obj, Value alist)
{
    expect_type("assq", TYPE_PAIR, alist);
    return assq(obj, alist);
}

static Value assoc(Value key, Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        Value entry = car(p);
        if (value_is_pair(entry) && equal(car(entry), key))
            return entry;
    }
    return Qfalse;
}

static Value proc_assoc(UNUSED Value *env, Value obj, Value alist)
{
    expect_type("assoc", TYPE_PAIR, alist);
    return assoc(obj, alist);
}

// 6.3.3. Symbols
static Value proc_symbol_p(UNUSED Value *env, Value obj)
{
    return OF_BOOL(value_is_symbol(obj));
}

// 6.3.5. Strings
static Value proc_string_p(UNUSED Value *env, Value obj)
{
    return OF_BOOL(value_is_string(obj));
}

static Value proc_string_length(UNUSED Value *env, Value s)
{
    expect_type("string-length", TYPE_STR, s);
    return value_of_int(strlen(STRING(s)->body));
}

static Value proc_string_eq(UNUSED Value *env, Value s1, Value s2)
{
    expect_type_twin("string=?", TYPE_STR, s1, s2);
    return OF_BOOL(strcmp(STRING(s1)->body, STRING(s2)->body) == 0);
}

// 6.4. Control features
static Value proc_procedure_p(UNUSED Value *env, Value o)
{
    return OF_BOOL(value_is_procedure(o));
}

static Value build_apply_args(Value args)
{
    Value heads = Qnil, last = Qnil, p, next;
    for (p = args; (next = cdr(p)) != Qnil; p = next) {
        last = append_at(last, car(p));
        if (heads == Qnil)
            heads = last;
    }
    Value rest = car(p);
    expect_type("args on apply", TYPE_PAIR, rest);
    return append2(heads, rest);
}

static Value proc_apply(Value *env, Value args)
{
    expect_arity_range("apply", 2, -1, args);

    Value proc = car(args);
    expect_type("apply", TYPE_PROC, proc);
    Value appargs = build_apply_args(cdr(args));
    return apply(env, proc, appargs);
}

static bool cars_cdrs(Value ls, Value *pcars, Value *pcdrs)
{
    Value lcars = Qnil, lcdrs = Qnil;
    Value cars = Qnil , cdrs = Qnil;
    for (Value p = ls; p != Qnil; p = cdr(p)) {
        Value l = car(p);
        expect_type("map", TYPE_PAIR, l);
        if (l == Qnil)
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

static Value proc_map(Value *env, Value args)
{
    expect_arity_range("map", 2, -1, args);

    Value proc = car(args);
    expect_type("map", TYPE_PROC, proc);
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

static Value proc_for_each(Value *env, Value args)
{
    expect_arity_range("for-each", 2, -1, args);

    Value proc = car(args);
    expect_type("for-each", TYPE_PROC, proc);
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
    return (Value) c;
}

ATTR(noinline)
static bool continuation_set(Value c)
{
    GET_SP(sp); // must be the first!
    Continuation *cont = CONTINUATION(c);
    cont->sp = sp;
    cont->shelter_len = stack_base - sp;
    cont->shelter = xmalloc(cont->shelter_len);
    memcpy(cont->shelter, (void *) sp, cont->shelter_len);
    cont->call_stack = call_stack;
    return setjmp(cont->state) != 0;
}

static Value proc_callcc(Value *env, Value proc)
{
    expect_type("call/cc", TYPE_PROC, proc);
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
    case TYPE_BOOL:
        fprintf(f, "%s", v == Qtrue ? "#t" : "#f");
        break;
    case TYPE_INT:
        fprintf(f, "%"PRId64, value_to_int(v));
        break;
    case TYPE_SYMBOL:
    case TYPE_STR:
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
        return NULL;
    fdisplay(stream, v);
    fclose(stream);
    return s;
}

void display(Value v)
{
    fdisplay(stdout, v);
}

static Value proc_display(UNUSED Value *env, Value obj)
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
static Value proc_load(UNUSED Value *env, Value path)
{
    // Current spec: path is always relative
    return load_inner(value_to_string(path));
}

// Local Extensions
static Value proc_print(UNUSED Value *env, Value obj)
{
    display(obj);
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

static Value syn_defined_p(Value *env, Value name)
{
    if (!value_is_symbol(name))
        return Qfalse;
    return OF_BOOL(assq(name, *env) != Qfalse);
}

#define DEF_CXR_BUILTIN(x, y) \
    static Value proc_c##x##y##r(UNUSED Value *env, Value v) \
    { \
        expect_type("c" #x #y "r", TYPE_PAIR, v); \
        return c##x##y##r(v); \
    }
CXRS(DEF_CXR_BUILTIN)

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
    SYM_RARROW = value_of_symbol("=>");

    Value *e = &toplevel_environment;

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

    // Local Extensions
    define_procedure(e, "print", proc_print, 1); // like Gauche
    define_procedure(e, "_cputime", proc_cputime, 0);
    define_syntax(e, "_defined?", syn_defined_p, 1);
}
