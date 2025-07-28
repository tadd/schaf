#include <ctype.h>
#include <inttypes.h>
#include <setjmp.h>
#include <stdarg.h>
#include <string.h>

#include "intern.h"
#include "schaf.h"
#include "utils.h"

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
    TOK_TYPE_CONST,
    TOK_TYPE_EOF
} TokenType;

typedef struct {
    TokenType type;
    Value value;
} Token;

// singletons
#define DEF_TOKEN(name) static const Token TOK_##name = { .type = TOK_TYPE_##name }
DEF_TOKEN(LPAREN);
DEF_TOKEN(RPAREN);
DEF_TOKEN(QUOTE);
DEF_TOKEN(GRAVE);
DEF_TOKEN(COMMA);
DEF_TOKEN(SPLICE);
DEF_TOKEN(DOT);
static const Token TOK_EOF = { .type = TOK_TYPE_EOF }; // avoid conflict with stdio.h

// and ctors
#define TOK_V(t, v) ((Token) { .type = TOK_TYPE_ ## t, .value = v })
static inline Token TOK_INT(int64_t i)
{
    return TOK_V(INT, value_of_int(i));
}
static inline Token TOK_STRING(const char *s)
{
    return TOK_V(STRING, value_of_string(s));
}
static inline Token TOK_IDENT(const char *s)
{
    return TOK_V(IDENT, value_of_symbol(s));
}
static inline Token TOK_CONST(Value c)
{
    return TOK_V(CONST, c);
}

typedef struct {
    FILE *in;
    const char *filename;
    Value newline_pos; // list of pos | int
    jmp_buf jmp_error;
} Parser;

void pos_to_line_col(int64_t pos, Value newline_pos, int64_t *line, int64_t *col)
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

static void get_line_and_column(const Parser *p, int64_t *line, int64_t *col)
{
    int64_t pos = ftell(p->in);
    Value newline_pos = reverse(p->newline_pos);
    pos_to_line_col(pos, newline_pos, line, col);
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
    p->newline_pos = cons(value_of_int(ftell(p->in)), p->newline_pos);
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
        return TOK_IDENT("..");
    }
    return TOK_IDENT("...");
}

static Token lex_string(Parser *p)
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
    return TOK_STRING(buf);
}

static Token lex_constant(Parser *p)
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

static Token lex_int(Parser *p, int c, int sign)
{
    ungetc(c, p->in);
    int64_t i;
    int n = fscanf(p->in, "%"SCNd64, &i);
    if (n != 1)
        parse_error(p, "integer", "invalid string");
    return TOK_INT(sign * i);
}

static Token lex_after_sign(Parser *p, int csign)
{
    int c = fgetc(p->in);
    int dig = isdigit(c);
    if (dig) {
        int sign = csign == '-' ? -1 : 1;
        return lex_int(p, c, sign);
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
    return TOK_IDENT(buf);
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
    if (isdigit(c))
        return lex_int(p, c, 1);
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
    case TOK_TYPE_INT:
        snprintf(buf, sizeof(buf), "%"PRId64, value_to_int(t.value));
        break;
    case TOK_TYPE_IDENT:
        return value_to_string(t.value);
    case TOK_TYPE_STRING:
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
        if (first && value_is_symbol(e))
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
    case TOK_TYPE_STRING:
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
    p->newline_pos = Qnil;
    return p;
}

static inline Value list3_const(Value x, Value y, Value z)
{
    return cons_const(x, list2_const(y, z));
}

// AST: (filename syntax_list newline_positions)
static Value ast_new(const Parser *p, Value syntax_list)
{
    Value filename = value_of_string(p->filename);
    return list3_const(filename, syntax_list, reverse(p->newline_pos));
}

static Value parse_program(Parser *p)
{
    Value v = DUMMY_PAIR();
    for (Value last = v, expr; (expr = parse_expr(p)) != Qundef; )
        last = PAIR(last)->cdr = list1(expr);
    return ast_new(p, cdr(v));
}

Value iparse(FILE *in, const char *filename)
{
    Parser *p = parser_new(in, filename);
    Value ast;
    if (setjmp(p->jmp_error) == 0)
        ast = parse_program(p); // success
    else
        ast = Qundef; // got an error
    free(p);
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
    free(p);
    return datum;
}


Value parse(const char *path)
{
    FILE *in = fopen(path, "r");
    if (in == NULL)
        error("parse: can't open file: %s", path);
    Value ast = iparse(in, path);
    fclose(in);
    return ast == Qundef ? Qundef : car(cdr(ast));
}

Value parse_string(const char *in)
{
    FILE *f = fmemopen((char *) in, strlen(in), "r");
    Value ast = iparse(f, "<inline>");
    fclose(f);
    return ast == Qundef ? Qundef : car(cdr(ast));
}
