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
    TOK_TYPE_TRUE,
    TOK_TYPE_FALSE,
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
static const Token TOK_TRUE = TOKEN_C(TRUE);
static const Token TOK_FALSE = TOKEN_C(FALSE);
static const Token TOK_VECTOR_LPAREN = TOKEN_C(VECTOR_LPAREN);
static const Token TOK_EOF = { .type = TOK_TYPE_EOF }; // avoid conflict with "EOF" in stdio.h
static Token TOK_DOT2_v = TOKEN_C(IDENT);
static Token TOK_DOT3_v = TOKEN_C(IDENT);
static Token TOK_PLUS_v = TOKEN_C(IDENT);
static Token TOK_MINUS_v = TOKEN_C(IDENT);
#define DEF_CONST_TOKEN_FUNC(name, str) \
    static inline Token TOK_##name(void) \
    { \
        if (TOK_##name##_v.value == 0) \
            TOK_##name##_v.value = sch_symbol_new(str); \
        return TOK_##name##_v; \
    }
DEF_CONST_TOKEN_FUNC(DOT2, "..")
DEF_CONST_TOKEN_FUNC(DOT3, "...")
DEF_CONST_TOKEN_FUNC(PLUS, "+")
DEF_CONST_TOKEN_FUNC(MINUS, "-")

// and ctor-s
static inline Token token_int(int64_t i)
{
    return TOKEN_VAL(INT, sch_integer_new(i));
}
static inline Token token_string(const char *s)
{
    return TOKEN_VAL(STRING, sch_string_new(s));
}
static inline Token token_ident(const char *s)
{
    return TOKEN_VAL(IDENT, sch_symbol_new(s));
}

typedef struct {
    FILE *in;
    int64_t *newline_pos;
    jmp_buf jmp_error;
    char filename[];
} Parser;

void pos_to_line_col(int64_t pos, const int64_t *newline_pos, int64_t *line, int64_t *col)
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
        return TOK_DOT2();
    }
    return TOK_DOT3();
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
    return token_string(buf);
}

static Token lex_int_binary(Parser *p, int coeff)
{
    int64_t v = 0;
    size_t i;
    for (i = 0; i < 63; i++) {
        int c = fgetc(p->in);
        if (c == '0')
            v <<= 1U;
        else if (c == '1')
            v = (v << 1U) | 1U;
        else if (isalnum(c)) // XXX
            parse_error(p, "binary digits", "'%c'", c);
        else {
            ungetc(c, p->in);
            break;
        }
    }
    if (i == 0 || i == 63)
        parse_error(p, "integer", "invalid string");
    return token_int(coeff * v);
}

static Token lex_int_with_radix(Parser *p, int coeff, unsigned radix)
{
    switch (radix) {
    case 2: case 8: case 10: case 16:
        break;
    default:
        bug("invalid radix");
    }
    if (radix == 2)
        return lex_int_binary(p, coeff);
    char *s;
    int n = fscanf(p->in, "%m[0-9a-zA-Z]", &s);
    if (n != 1)
        parse_error(p, "integer digits", "nothing");
    char *endp;
    int64_t i = strtol(s, &endp, radix);
    if (endp[0] != '\0')
        parse_error(p, "integer digits", "'%s'", endp); // XXX
    free(s);
    return token_int(coeff * i);
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
        return TOK_TRUE;
    case 'f':
        return TOK_FALSE;
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
    int64_t i;
    int n = fscanf(p->in, "%"PRId64, &i);
    if (n != 1)
        parse_error(p, "integer digits", "invalid string");
    return token_int(coeff * i);
}

static Token lex_after_sign(Parser *p, int csign)
{
    int c = fgetc(p->in);
    int dig = isdigit(c);
    ungetc(c, p->in);
    bool minus = csign == '-';
    if (dig)
        return lex_int(p, minus * -2 + 1);
    return minus ? TOK_MINUS() : TOK_PLUS();
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

static bool is_keyword(const char *id);

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
    if (is_keyword(buf))
        debug("got a keyword: %s", buf);
    return token_ident(buf);
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
        snprintf(buf, sizeof(buf), "%"PRId64, sch_integer_to_cint(t.value));
        break;
    case TOK_TYPE_IDENT:
        return sch_symbol_to_cstr(t.value);
    case TOK_TYPE_STRING:
        snprintf(buf, sizeof(buf), "\"%s\"", STRING(t.value));
        break;
    case TOK_TYPE_TRUE:
        return "#t";
    case TOK_TYPE_FALSE:
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
    LocatedPair *p = obj_new(TAG_PAIR, sizeof(LocatedPair)); // imitate ordinal pairs
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
    case TOK_TYPE_TRUE:
        return Qtrue;
    case TOK_TYPE_FALSE:
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
    size_t len = strlen(filename);
    Parser *p = xmalloc(sizeof(Parser) + len + 1);
    p->in = in;
    p->newline_pos = scary_new(sizeof(int64_t));
    strcpy(p->filename, filename);
    return p;
}

static void parser_free(Parser *p)
{
    if (p == NULL)
        return;
    scary_free(p->newline_pos);
    free(p);
}

// Source: filename, ast, newline_pos
static Source *source_new(Parser *p, Value syntax_list)
{
    size_t len = strlen(p->filename);
    Source *src = xmalloc(sizeof(Source) + len + 1);
    src->ast = syntax_list;
    src->newline_pos = p->newline_pos; // move
    p->newline_pos = NULL;
    strcpy(src->filename, p->filename);
    return src;
}

void source_free(Source *s)
{
    if (s == NULL)
        return;
    scary_free(s->newline_pos);
    free(s);
}

#if 1
static void strtou64x2(const char *s, uint64_t val[2])
{
    uint8_t buf[16] = { 0 };
    strncpy((char *) buf, s, 16);
    val[0] = *(uint64_t *) buf;
    val[1] = *(uint64_t *) (buf + 8);
}

static void init_keywords(uint64_t keywords[][2])
{
    const char *words[] = {
        "=>",
        "and",
        "begin",
        "case",
        "cond",
        "define",
        "delay",
        "do",
        "else",
        "if",
        "lambda",
        "let",
        "let*",
        "letrec",
        "or",
        "quasiquote",
        "quote",
        "set!",
        "unquote",
        "unquote-splicing"
    };
    for (size_t i = 0; i < sizeof(words) / sizeof(words[0]); i++)
        strtou64x2(words[i], keywords[i]);
}

static bool is_keyword(const char *id)
{
    static uint64_t keywords[20][2] = { 0 };
    if (keywords[0][0] == 0)
        init_keywords(keywords);
    size_t len = strlen(id);
    if (len < 2 || (len > 7 && len != 10 && len != 16))
        return false;
    uint64_t val[2];
    strtou64x2(id, val);
    for (size_t i = 0; i < 20; i++) {
        uint64_t *key = keywords[i];
        if (key[0] == val[0] && key[1] == val[1])
            return true;
    }
    return false;
}

#elif 1
static bool getkeyword(const char *id, uint64_t val[2])
{
#define RANGE(x, y) ((x)+1)...((y)-1) // open interval
    static const uint8_t table[256] = {
        [RANGE(-1, '!')] = 255,
        ['!'] = 0,
        [RANGE('!', '*')] = 255,
        ['*'] = 1,
        [RANGE('*', '-')] = 255,
        ['-'] = 2,
        [RANGE('-', '=')] = 255,
        ['='] = 3,
        ['>'] = 4,
        [RANGE('>', 'a')] = 255,
        ['a'] = 5,
        ['b'] = 6,
        ['c'] = 7,
        ['d'] = 8,
        ['e'] = 9,
        ['f'] = 10,
        ['g'] = 11,
        ['h'] = 255,
        ['i'] = 12,
        ['j'] = 255,
        ['k'] = 255,
        ['l'] = 13,
        ['m'] = 14,
        ['n'] = 15,
        ['o'] = 16,
        ['p'] = 17,
        ['q'] = 18,
        ['r'] = 19,
        ['s'] = 20,
        ['t'] = 21,
        ['u'] = 22,
        ['v'] = 255,
        ['w'] = 255,
        ['x'] = 255,
        ['y'] = 23,
        ['z'] = 255,
        [RANGE('z', 256)] = 255,
    };
    uint64_t v = 0;
    for (size_t i = 0; i < 13; i++) {
        uint8_t ch = (uint8_t) id[i];
        if (ch == '\0') {
            val[0] = v;
            val[1] = UINT64_MAX;
            return true;
        }
        uint64_t n = table[ch];
        if (n == 255)
            return false;
        v = v * 24U + n;
    }
    uint64_t v2 = 0;
    for (size_t i = 13; i <= 16; i++) {
        uint8_t ch = (uint8_t) id[i];
        if (ch == '\0') {
            val[0] = v;
            val[1] = v2;
            return true;
        }
        uint64_t n = table[ch];
        if (n == 255)
            return false;
        v2 = v2 * 24U + n;
    }
    return false;
}

static void init_keywords(uint64_t keywords[20][2])
{
    const char *words[] = {
        "=>",
        "and",
        "begin",
        "case",
        "cond",
        "define",
        "delay",
        "do",
        "else",
        "if",
        "lambda",
        "let",
        "let*",
        "letrec",
        "or",
        "quasiquote",
        "quote",
        "set!",
        "unquote",
        "unquote-splicing"
    };
    for (size_t i = 0; i < sizeof(words) / sizeof(words[0]); i++) {
        if (!getkeyword(words[i], keywords[i]))
            bug("keyword caching failed");
    }
}

static bool is_keyword(const char *id)
{
    static uint64_t keywords[20][2] = { 0 };
    if (keywords[0][0] == 0)
        init_keywords(keywords);
    uint64_t val[2];
    if (!getkeyword(id, val))
        return false;
    for (size_t i = 0; i < 20; i++) {
        uint64_t *key = keywords[i];
        if (key[0] == val[0] && key[1] == val[1])
            return true;
    }
    return false;
}

#else

static int getkeyword(const char *id)
{
    switch (*id++) {
    case '=':
        if (*id == '>')
            return 1;
        break;
    case 'a':
        if (strcmp(id, "nd") == 0)
            return 2;
        break;
    case 'b':
        if (strcmp(id, "egin") == 0)
            return 3;
        break;
    case 'c':
        if (strcmp(id, "ase") == 0)
            return 4;
        if (strcmp(id, "ond") == 0)
            return 5;
        break;
    case 'd':
        switch (*id++) {
        case 'e':
            if (strcmp(id, "fine") == 0)
                return 6;
            if (strcmp(id, "lay") == 0)
                return 7;
            break;
        case 'o':
            if (*id == '\0')
                return 8;
            break;
        default:
            break;
        }
        break;
    case 'e':
        if (strcmp(id, "lse") == 0)
            return 9;
        break;
    case 'i':
        if (strcmp(id, "f") == 0)
            return 10;
        break;
    case 'l':
        switch (*id++) {
        case 'a':
            if (strcmp(id, "mbda") == 0)
                return 11;
            break;
        case 'e':
            switch (*id++) {
            case 't':
                switch (*id++) {
                case '\0':
                    return 12;
                case '*':
                    if (*id == '\0')
                        return 13;
                    break;
                case 'r':
                    if (strcmp(id, "ec") == 0)
                        return 14;
                    break;
                default:
                    break;
                }
            default:
                break;
            }
        default:
            break;
        }
        break;
    case 'o':
        if (strcmp(id, "r") == 0)
            return 15;
        break;
    case 'q':
        if (*id++ != 'u')
            break;
        if (strcmp(id, "asiquote") == 0)
            return 16;
        if (strcmp(id, "ote") == 0)
            return 17;
        break;
    case 's':
        if (strcmp(id, "et!") == 0)
            return 18;
        break;
    case 'u':
        if (strncmp(id, "nquote", 6) != 0)
            break;
        id += 6;
        if (*id == '\0')
            return 19;
        if (strcmp(id, "-splicing") == 0)
            return 20;
        break;
    default:
        break;
    }
    return 0;
}

static bool is_keyword(const char *id)
{
    return getkeyword(id) > 0;
}
#endif

#if 0
static bool is_expression_keyword(Value ident)
{
    return ident == SYM_QUOTE || ident == SYM_LAMBDA || ident == SYM_IF ||
        ident == SYM_SET_BANG || ident == SYM_BEGIN || ident == SYM_COND ||
        ident == SYM_AND || ident == SYM_OR || ident == SYM_CASE ||
        ident == SYM_LET || ident == SYM_LET_STAR || ident == SYM_LETREC ||
        ident == SYM_DO || ident == SYM_DELAY || ident == SYM_QUASIQUOTE;
}

static bool is_syntactic_keyword(Value ident)
{
    return is_expression_keyword(ident) ||
        ident == SYM_ELSE || ident == SYM_RARROW || ident == SYM_DEFINE ||
        ident == SYM_UNQUOTE || ident == SYM_UNQUOTE_SPLICING;
}
#endif

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
    FILE *f = mopen(in);
    Value ast = iparse_ast(f, "<inline>");
    fclose(f);
    return ast;
}
