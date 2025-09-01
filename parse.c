#include <ctype.h>
#include <inttypes.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>

#include "intern.h"
#include "libscary.h"
#include "schaf.h"
#include "utils.h"

enum {
    INT64_MAX_DIG2 = 63,
    INT64_MAX_DIG8 = 21,
    INT64_MAX_DIG10 = 18,
    INT64_MAX_DIG16 = 15,
};

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
    TOK_TYPE_CONST_TRUE,
    TOK_TYPE_CONST_FALSE,
    TOK_TYPE_VECTOR_LPAREN,
    TOK_TYPE_EOF
} TokenType;

typedef struct {
    TokenType type;
    Value value;
} Token;

// singletons
#define TOKEN_VAL(t, v) ((Token) { .type = TOK_TYPE_ ## t, .value = v })
#define TOKEN_C(t) TOKEN_VAL(t, 0)
static const Token TOK_LPAREN = TOKEN_C(LPAREN);
static const Token TOK_RPAREN = TOKEN_C(RPAREN);
static const Token TOK_QUOTE = TOKEN_C(QUOTE);
static const Token TOK_GRAVE = TOKEN_C(GRAVE);
static const Token TOK_COMMA = TOKEN_C(COMMA);
static const Token TOK_SPLICE = TOKEN_C(SPLICE);
static const Token TOK_DOT = TOKEN_C(DOT);
static const Token TOK_CONST_TRUE = TOKEN_C(CONST_TRUE);
static const Token TOK_CONST_FALSE = TOKEN_C(CONST_FALSE);
static const Token TOK_VECTOR_LPAREN = TOKEN_C(VECTOR_LPAREN);
static const Token TOK_EOF = { .type = TOK_TYPE_EOF }; // avoid conflict with "EOF" in stdio.h
static Token tok_ident_dot2 = TOKEN_C(IDENT);
static Token tok_ident_dot3 = TOKEN_C(IDENT);
static Token tok_ident_plus = TOKEN_C(IDENT);
static Token tok_ident_minus = TOKEN_C(IDENT);
#define DEF_CONST_TOKEN_FUNC(lower, upper, val) \
    static inline Token TOK_##upper(void) \
    { \
        if (tok_##lower.value == 0) \
            tok_##lower.value = val; \
        return tok_##lower; \
    }
DEF_CONST_TOKEN_FUNC(ident_dot2, IDENT_DOT2, sch_symbol_new(".."))
DEF_CONST_TOKEN_FUNC(ident_dot3, IDENT_DOT3, sch_symbol_new("..."))
DEF_CONST_TOKEN_FUNC(ident_plus, IDENT_PLUS, sch_symbol_new("+"))
DEF_CONST_TOKEN_FUNC(ident_minus, IDENT_MINUS, sch_symbol_new("-"))

// and ctor-s
static inline Token TOKEN_INT_FIXNUM(int64_t i)
{
    return TOKEN_VAL(INT, fixnum_normalize(i));
}

static inline Token TOKEN_INT_BIGNUM(BigInt *i, bool negative)
{
    if (negative) {
        BigInt *tmp = bigint_negate(i);
        bigint_free(i);
        i = tmp;
    }
    return TOKEN_VAL(INT, bignum_normalize(i));
}
static inline Token TOKEN_STRING(const char *s)
{
    return TOKEN_VAL(STRING, sch_string_new(s));
}
static inline Token TOKEN_IDENT(const char *s)
{
    return TOKEN_VAL(IDENT, sch_symbol_new(s));
}

typedef struct {
    FILE *in;
    char *filename;
    int64_t *newline_pos;
    jmp_buf jmp_error;
} Parser;

void pos_to_line_col(int64_t pos, int64_t *newline_pos, int64_t *line, int64_t *col)
{
    int64_t nline, last = 0, len = scary_length(newline_pos);
    for (nline = 0; nline < len; nline++) {
        int64_t n = newline_pos[nline];
        if (n > pos)
            break;
        last = n;
    }
    *line = nline + 1;
    *col = pos - last + 1;
}

static void get_line_and_column(const Parser *p, int64_t *line, int64_t *col)
{
    int64_t pos = ftell(p->in);
    pos_to_line_col(pos, p->newline_pos, line, col);
}

#define parse_error(p, exp, act, ...) do { \
        int64_t line, col; \
        get_line_and_column(p, &line, &col); \
        raise_error(p->jmp_error, \
                    "%s:%"PRId64":%"PRId64": expected %s but got " act, \
                    p->filename, line, col, exp __VA_OPT__(,) __VA_ARGS__); \
    } while (0)

static inline void put_newline_pos(Parser *p)
{
    int64_t pos = ftell(p->in);
    scary_push(&p->newline_pos, pos);
}

static void skip_comment(Parser *p)
{
    int c;
    while ((c = fgetc(p->in)) != EOF) {
        if (c == '\n') {
            put_newline_pos(p);
            return;
        }
    }
}

static void skip_token_atmosphere(Parser *p)
{
    int c;
    while ((c = fgetc(p->in)) != EOF) {
        if (c == ';') {
            skip_comment(p);
            continue;
        }
        if (!isspace(c))
            break;
        if (c == '\n')
            put_newline_pos(p);
    }
    ungetc(c, p->in);
}

static Token lex_comma_or_splice(Parser *p)
{
    int c = fgetc(p->in);
    if (c == '@')
        return TOK_SPLICE;
    ungetc(c, p->in);
    return TOK_COMMA;
}

static Token lex_dots(Parser *p)
{
    int c = fgetc(p->in);
    if (c != '.') {
        ungetc(c, p->in);
        return TOK_DOT;
    }
    c = fgetc(p->in);
    if (c != '.') {
        ungetc(c, p->in);
        return TOK_IDENT_DOT2();
    }
    return TOK_IDENT_DOT3();
}

static int unescape_hex(Parser *p)
{
    unsigned u;
    int n = fscanf(p->in, "%x;", &u);
    const char *exp = "hex escape in string literal";
    if (n == EOF)
        parse_error(p, exp, "EOF");
    if (n != 1)
        parse_error(p, exp, "invalid string");
    if (u == 0)
        parse_error(p, exp, "NUL character which forbidden (as of now)");
    if (u > 0xffU)
        parse_error(p, exp, "too large value: %u", u);
    return u;
}

static int unescape(Parser *p, int c)
{
    switch (c) {
    case 'a':
        return '\a';
    case 'b':
        return '\b';
    case 't':
        return '\t';
    case 'r':
        return '\r';
    case 'n':
        return '\n';
    case 'x':
        return unescape_hex(p);
    case '\\':
    case '"':
    case '|':
        return c; // as is
    default:
        parse_error(p, "escape in string literal", "unknown: '\\%c'", c);
    }
}

static Token lex_string(Parser *p)
{
    char buf[BUFSIZ], *pbuf = buf, *end = pbuf + sizeof(buf) - 2;
    for (int c; (c = fgetc(p->in)) != '"'; *pbuf++ = c) {
        if (c == '\\') {
            c = fgetc(p->in);
            c = unescape(p, c);
        }
        if (pbuf == end)
            parse_error(p, "string literal", "too long: \"%s...\"", pbuf);
    }
    *pbuf = '\0';
    return TOKEN_STRING(buf);
}

static Token lex_int_with_radix(Parser *p, int coeff, unsigned radix)
{
    size_t max_dig;
    BigInt *(*bigint_from_s)(const char *s);
    switch (radix) {
    case 2:
        max_dig = INT64_MAX_DIG2;
        bigint_from_s = bigint_from_string_bin;
        break;
    case 8:
        max_dig = INT64_MAX_DIG8;
        bigint_from_s = bigint_from_string_oct;
        break;
    case 10:
        max_dig = INT64_MAX_DIG10;
        bigint_from_s = bigint_from_string;
        break;
    case 16:
        max_dig = INT64_MAX_DIG16;
        bigint_from_s = bigint_from_string_hex;
        break;
    default:
        bug("invalid radix");
    }
    char *s;
    int n = fscanf(p->in, "%m[0-9a-zA-Z]", &s);
    if (n != 1)
        parse_error(p, "integer digits", "nothing");
    if (strlen(s) <= max_dig) {
        char *endp;
        int64_t i = strtol(s, &endp, radix);
        if (endp[0] != '\0')
            parse_error(p, "integer digits",
                        "'%s' which invalid in radix %u", endp, radix); // XXX
        free(s);
        return TOKEN_INT_FIXNUM(coeff * i);
    }
    BigInt *b = bigint_from_s(s);
    if (b == NULL)
        parse_error(p, "integer digits",
                    "'%s' which invalid in radix %u", s, radix); // XXX
    free(s);
    return TOKEN_INT_BIGNUM(b, coeff < 0);
}

static Token lex_signed_int_with_radix(Parser *p, unsigned radix)
{
    int coeff = 1;
    int c = fgetc(p->in);
    if (c == '+')
        ; // just ignore
    else if (c == '-')
        coeff = -1;
    else
        ungetc(c, p->in);
    return lex_int_with_radix(p, coeff, radix);
}

static Token lex_constant(Parser *p)
{
    unsigned radix;
    int c = fgetc(p->in);
    switch (c) {
    case 't':
        return TOK_CONST_TRUE;
    case 'f':
        return TOK_CONST_FALSE;
    case '(':
        return TOK_VECTOR_LPAREN;
    case 'b':
        radix = 2;
        break;
    case 'd':
        radix = 10;
        break;
    case 'o':
        radix = 8;
        break;
    case 'x':
        radix = 16;
        break;
    default:
        parse_error(p, "constants", "#%c", c);
    }
    return lex_signed_int_with_radix(p, radix);
}

static Token lex_int(Parser *p, int coeff)
{
    char *s;
    int n = fscanf(p->in, "%m[0-9a-zA-Z]", &s);
    if (n != 1)
        parse_error(p, "integer digits", "nothing");
    if (strlen(s) > INT64_MAX_DIG10) {
        BigInt *b = bigint_from_string(s);
        if (b == NULL)
            parse_error(p, "integer digits", "'%s'", s);
        free(s);
        return TOKEN_INT_BIGNUM(b, coeff < 0);
    }
    char *endp;
    int64_t i = strtol(s, &endp, 10);
    if (endp[0] != '\0')
        parse_error(p, "integer digits", "'%s'", endp);
    free(s);
    return TOKEN_INT_FIXNUM(coeff * i);
}

static Token lex_after_sign(Parser *p, int csign)
{
    int c = fgetc(p->in);
    int dig = isdigit(c);
    ungetc(c, p->in);
    bool minus = csign == '-';
    if (dig) {
        int coeff = minus ? -1 : 1;
        return lex_int(p, coeff);
    }
    return minus ? TOK_IDENT_MINUS() : TOK_IDENT_PLUS();
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

static Token lex_ident(Parser *p, int init)
{
    char buf[BUFSIZ], *s = buf, *end = s + sizeof(buf);
    int c;
    for (*s++ = init; is_subsequent(c = fgetc(p->in)); *s++ = c) {
        if (s == end)
            parse_error(p, "identifier", "too long");
    }
    ungetc(c, p->in);
    *s = '\0';
    return TOKEN_IDENT(buf);
}

static Token lex(Parser *p)
{
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
        return lex_comma_or_splice(p);
    case '.':
        return lex_dots(p);
    case '"':
        return lex_string(p);
    case '#':
        return lex_constant(p);
    case '+':
    case '-':
        return lex_after_sign(p, c);
    case EOF:
        return TOK_EOF;
    default:
        break;
    }
    if (isdigit(c)) {
        ungetc(c, p->in);
        return lex_int(p, 1);
    }
    if (is_initial(c))
        return lex_ident(p, c);
    parse_error(p, "valid char", "'%c'", c);
}

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
    case TOK_TYPE_VECTOR_LPAREN:
        return "#(";
    case TOK_TYPE_INT:
        if (!sch_value_is_fixnum(t.value))
            return bigint_to_string(BIGNUM(t.value));
        snprintf(buf, sizeof(buf), "%"PRId64, sch_fixnum_to_cint(t.value));
        break;
    case TOK_TYPE_IDENT:
        return sch_symbol_to_cstr(t.value);
    case TOK_TYPE_STRING:
        snprintf(buf, sizeof(buf), "\"%s\"", STRING(t.value));
        break;
    case TOK_TYPE_CONST_TRUE:
        return "#t";
    case TOK_TYPE_CONST_FALSE:
        return "#f";
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
    Token t = lex(p);
    if (t.type != TOK_TYPE_RPAREN)
        parse_error(p, "')'", "'%s'", token_stringify(t));
    PAIR(last)->cdr = e;
    return l;
}

static Value located_list1(Value sym, int64_t pos)
{
    LocatedPair *p = obj_new(sizeof(LocatedPair), TAG_PAIR); // imitate ordinal pairs
    HEADER(p)->immutable = true;
    PAIR(p)->car = sym;
    PAIR(p)->cdr = Qnil;
    p->pos = pos;
    return (Value) p;
}

static Value parse_list(Parser *p)
{
    Value ret = DUMMY_PAIR(), last = ret;
    int64_t pos = ftell(p->in);
    for (;;) {
        skip_token_atmosphere(p);
        int c = fgetc(p->in);
        if (c == ')')
            break;
        if (c == EOF)
            parse_error(p, "')'", "EOF");
        if (c == '.')
            return parse_dotted_pair(p, cdr(ret), last);
        ungetc(c, p->in);
        Value e = parse_expr(p);
        bool first = (ret == last);
        Value l;
        if (first && sch_value_is_symbol(e))
            l = located_list1(e, pos);
        else
            l = list1_const(e);
        last = PAIR(last)->cdr = l;
    }
    return cdr(ret);
}

static Value parse_quoted(Parser *p, Value sym)
{
    Value e = parse_expr(p);
    if (e == Qundef)
        parse_error(p, "expression", "EOF");
    return list2_const(sym, e);
}

static Value parse_vector(Parser *p)
{
    Value v = vector_new();
    for (;;) {
        skip_token_atmosphere(p);
        int c = fgetc(p->in);
        if (c == ')')
            break;
        if (c == EOF)
            parse_error(p, "')'", "EOF");
        ungetc(c, p->in);
        vector_push(v, parse_expr(p));
    }
    return v;
}

static Value parse_expr(Parser *p)
{
    Token t = lex(p);
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
    case TOK_TYPE_VECTOR_LPAREN:
        return parse_vector(p); // parse til ')'
    case TOK_TYPE_CONST_TRUE:
        return Qtrue;
    case TOK_TYPE_CONST_FALSE:
        return Qfalse;
    case TOK_TYPE_STRING:
    case TOK_TYPE_INT:
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
    p->filename = xstrdup(filename);
    p->newline_pos = scary_new(sizeof(int64_t));
    return p;
}

static void parser_free(Parser *p)
{
    if (p == NULL)
        return;
    scary_free(p->newline_pos);
    free(p->filename);
    free(p);
}

// Source: filename, ast, newline_pos
static Source *source_new(Parser *p, Value syntax_list)
{
    Source *src = xmalloc(sizeof(Source));
    src->filename = xstrdup(p->filename);
    src->ast = syntax_list;
    src->newline_pos = scary_dup(p->newline_pos);
    return src;
}

void source_free(Source *s)
{
    if (s == NULL)
        return;
    scary_free(s->newline_pos);
    free(s->filename);
    free(s);
}

static Source *parse_program(Parser *p)
{
    Value v = DUMMY_PAIR();
    for (Value last = v, expr; (expr = parse_expr(p)) != Qundef; )
        last = PAIR(last)->cdr = list1(expr);
    return source_new(p, cdr(v));
}

Source *iparse(FILE *in, const char *filename)
{
    Parser *p = parser_new(in, filename);
    Source *src;
    if (setjmp(p->jmp_error) == 0)
        src = parse_program(p); // success
    else
        src = NULL; // got an error
    parser_free(p);
    return src;
}

static Value iparse_ast(FILE *in, const char *filename)
{
    Source *src = iparse(in, filename);
    if (src == NULL)
        return Qundef;
    Value ast = src->ast;
    source_free(src);
    return ast;
}

Value parse_datum(FILE *in, const char *filename)
{
    Parser *p = parser_new(in, filename);
    Value datum;
    if (setjmp(p->jmp_error) == 0)
        datum = parse_expr(p);
    else
        datum = Qundef;
    parser_free(p);
    return datum;
}

Value sch_parse(const char *path)
{
    FILE *in = fopen(path, "r");
    if (UNLIKELY(in == NULL))
        error("parse: can't open file: %s", path);
    Value ast = iparse_ast(in, path);
    fclose(in);
    return ast;
}

Value sch_parse_string(const char *in)
{
    FILE *f = fmemopen((char *) in, strlen(in), "r");
    Value ast = iparse_ast(f, "<inline>");
    fclose(f);
    return ast;
}
