#include <stdlib.h>
#include <string.h>

#include <criterion/criterion.h>
#include <criterion/new/assert.h>

#include "intern.h"
#include "schaf.h"

#define expect_stringify(exp, v) do { \
        char *s = sch_stringify(v); \
        cr_expect_str_eq(s, exp); \
        free(s); \
    } while (0)

#define value_idfunc list
#define V(x) \
    _Generic(x, int: sch_integer_new, Value: value_idfunc, \
             char *: sch_string_new, const char *: sch_string_new)(x)
#define expect_value_eq(expected, actual) do { \
        Value vexp = expected, vact = actual; \
        if (sch_value_is_integer(vexp)) \
            expect_int_eq(vexp, vact); \
        else if (sch_value_is_string(vexp)) \
            expect_vstr_eq(STRING(vexp), vact); \
        else if (sch_value_is_symbol(vexp)) \
            expect_vsym_eq(sch_symbol_to_cstr(vexp), vact); \
    } while (0)
#define expect_list_eq(expected, actual) do { \
        Value exp = expected, act = actual; \
        cr_expect(sch_value_is_pair(act)); \
        expect_int_eq(length(exp), length(act)); \
        for (; exp != Qnil; exp = cdr(exp), act = cdr(act)) \
            expect_value_eq(car(exp), car(act)); \
    } while (0)
#define expect_pair_eq(ecar, ecdr, act) do { \
        Value A = act; \
        cr_expect(sch_value_is_pair(A)); \
        expect_value_eq(V(ecar), car(A)); \
        expect_value_eq(V(ecdr), cdr(A)); \
    } while (0)

#define expect_int_eq(exp, act) cr_expect(eq(int, exp, act))
#define expect_str_eq(exp, act) cr_expect_str_eq(act, exp)
#define expect_error(pattern, v) do { \
        expect_int_eq(Qundef, v); \
        char *m = strstr(sch_error_message(), pattern); \
        cr_expect_not_null(m, "actual string:\n\"%s\"\ndid not include:\n\"%s\"", \
                           sch_error_message(), pattern); \
    } while (0)

#define expect_no_error(v) \
    cr_expect_neq(v, Qundef, "got error with a message: '%s'", sch_error_message())
#define expect_vx_eq(t, from, to, exp, act) do { \
        Value a = act; \
        expect_no_error(a); \
        expect_int_eq(t, sch_value_type_of(a)); \
        expect_##to##_eq(exp, sch_##from##_to_c##to(a)); \
    } while (0)
#define expect_x_eq_parsed(x, exp, act) expect_##x##_eq(exp, parse_expr_string(act))

#define expect_vint_eq(exp, act) expect_vx_eq(TYPE_INT, integer, int, exp, act)
#define expect_vstr_eq(exp, act) expect_vx_eq(TYPE_STRING, string, str, exp, act)
#define expect_vsym_eq(exp, act) expect_vx_eq(TYPE_SYMBOL, symbol, str, exp, act)

#define expect_vint_eq_parsed(exp, act) expect_x_eq_parsed(vint, exp, act)
#define expect_vstr_eq_parsed(exp, act) expect_x_eq_parsed(vstr, exp, act)
#define expect_vsym_eq_parsed(exp, act) expect_x_eq_parsed(vsym, exp, act)
#define expect_list_eq_parsed(exp, act) expect_x_eq_parsed(list, exp, act)
#define expect_pair_eq_parsed(ecar, ecdr, act) expect_pair_eq(ecar, ecdr, parse_expr_string(act))
#define expect_parse_error(exp, act) expect_error(exp, parse_expr_string(act))
#define expect_runtime_error(exp, act) expect_error(exp, sch_eval_string_single(act))

static void test_sch_init(void)
{
    SCH_INIT(); // XXX
}
static void test_sch_fin(void)
{
    sch_fin();
}
TestSuite(schaf, .init = test_sch_init, .fini = test_sch_fin);

static Value parse_expr_string(const char *in)
{
    Value v = sch_parse_string(in);
    if (v == Qundef || v == Qnil)
        return v;
    return car(v);
}

// terminate with Qundef
static Value list(Value arg, ...)
{
    if (arg == Qundef)
        return Qnil;
    Value o, ret = list1(arg), last = ret;
    va_list ap;
    va_start(ap, arg);
    while ((o = va_arg(ap, Value)) != Qundef)
        last = PAIR(last)->cdr = list1(o);
    va_end(ap);
    return ret;
}

Test(schaf, printing) {
    expect_stringify("#t", Qtrue);
    expect_stringify("#f", Qfalse);
    expect_stringify("<undef>", Qundef);
    expect_stringify("()", Qnil);

    expect_stringify("0", sch_integer_new(0));
    expect_stringify("42", sch_integer_new(42));
    expect_stringify("-42", sch_integer_new(-42));

    expect_stringify("foo", sch_symbol_new("foo"));

    expect_stringify("(1)", cons(V(1), Qnil));
    expect_stringify("(1 . 2)", cons(V(1), V(2)));
    expect_stringify("(1 2)", list(V(1), V(2), Qundef));
}

Test(schaf, parse_int) {
    expect_vint_eq_parsed(42, "42");
    expect_vint_eq_parsed(-42, "-42");
}

Test(schaf, parse_prefixed_int) {
    expect_vint_eq_parsed(42, "#b101010");
    expect_vint_eq_parsed(42, "#b+101010");
    expect_vint_eq_parsed(-42, "#b-101010");

    expect_vint_eq_parsed(42, "#d42");
    expect_vint_eq_parsed(42, "#d+42");
    expect_vint_eq_parsed(-42, "#d-42");

    expect_vint_eq_parsed(42, "#o52");
    expect_vint_eq_parsed(42, "#o+52");
    expect_vint_eq_parsed(-42, "#o-52");

    expect_vint_eq_parsed(42, "#x2a");
    expect_vint_eq_parsed(42, "#x+2a");
    expect_vint_eq_parsed(-42, "#x-2a");
}

Test(schaf, parse_nil) {
    cr_expect(parse_expr_string("()") == Qnil);
}

Test(schaf, parse_list) {
    expect_list_eq_parsed(list(V(1), V(2), Qundef), "(1 2)");
}

Test(schaf, parse_string) {
    expect_vstr_eq_parsed("abc", "\"abc\"");
    expect_vstr_eq_parsed("a\\b", "\"a\\\\b\"");
    expect_vstr_eq_parsed("a\"b", "\"a\\\"b\"");
    expect_vstr_eq_parsed("\a\b\t\r\n", "\"\\a\\b\\t\\r\\n\"");
    expect_vstr_eq_parsed("\\\"|", "\"\\\\\\\"\\|\"");
    expect_vstr_eq_parsed("a\x20,b", "\"a\\x20;,b\"");
}

Test(schaf, parse_string_list) {
    expect_list_eq_parsed(list(V("abc"), V("def"), Qundef),
                          "(\"abc\" \"def\")");
}

#define caaaar(x) (car(car(car(car(x)))))
Test(schaf, cxr) {
    expect_vint_eq(42, caaaar(parse_expr_string("((((42))))")));
}

#define expect_immutable_object_error(s) \
    expect_runtime_error("cannot modify immutable object", s)

Test(schaf, set_cxr) {
    expect_immutable_object_error("(set-car! '(1) 2)");
    expect_immutable_object_error("(set-cdr! '(1) '(2))");
    expect_immutable_object_error("(set-car! `(1) 2)");
    expect_immutable_object_error("(set-cdr! `(1) '(2))");
    expect_immutable_object_error("(set-car! `(,1) 2)");
    expect_immutable_object_error("(set-cdr! `(,1) '(2))");
    expect_immutable_object_error("(set-car! `(,@'(1)) 2)");
    expect_immutable_object_error("(set-cdr! `(,@'(1)) '(2))");
    expect_immutable_object_error("(set-car! `(,@`(1)) 2)");
    expect_immutable_object_error("(set-cdr! `(,@`(1)) '(2))");
}

Test(schaf, parse_ident) {
    expect_vsym_eq_parsed("a", "a");
}

Test(schaf, parse_dot) {
    expect_pair_eq_parsed(1, 2, "(1 . 2)");
}

Test(schaf, parse_peculiar) {
    expect_vint_eq_parsed(42, "+42");
    cr_expect(sch_value_is_symbol(parse_expr_string("+")));
}

Test(schaf, parse_lambda) {
    expect_list_eq_parsed(list(sch_symbol_new("lambda"), Qnil, V(42), Qundef),
                          "(lambda () 42)");
}

Test(schaf, parse_broken) {
    expect_parse_error("got EOF", "(");
    expect_parse_error("got EOF", "'");
}

Test(schaf, parse_error_line_column) {
    expect_parse_error("<inline>:1:3: ", "(1");
    expect_parse_error("<inline>:2:3: ", "\n(1");
    expect_parse_error("<inline>:2:5: ", "()\n()(1");
}

Test(schaf, runtime_error_line_column) {
    expect_runtime_error(
"unbound variable: h\n"
"\t<inline>:2:14 in 'g'\n"
"\t<inline>:1:14 in 'f'\n"
"\t<inline>:3:2 in <toplevel>"
,
"(define (f) (g))\n"
"(define (g) (h))\n"
"(f)");
}

Test(schaf, runtime_error_frames) {
    expect_runtime_error(
"-: expected integer but got procedure\n"
"\t<inline>:1:14 in 'f'\n"
"\t<inline>:2:2 in <toplevel>"
,
"(define (f) (- -))\n"
"(f)");
}

Test(schaf, runtime_error_frames2) {
    expect_runtime_error(
"unbound variable: g\n"
"\t<inline>:1:18 in 'f'\n"
"\t<inline>:2:2 in <toplevel>"
,
"(define (f) (map g '()))\n"
"(f)");
}

#define expect_unbound_var_error(col, s) \
    expect_runtime_error( \
    "unbound variable: x\n" /* x: fixed variable name */  \
    "\t<inline>:1:" #col " in <toplevel>", s)

Test(schaf, runtime_error_funcs) {
    expect_unbound_var_error(9, "(print (x))");
}

Test(schaf, runtime_error_symbol) {
    expect_runtime_error(
"unbound variable: h\n"
"\t<inline>:2:13 in 'g'\n"
"\t<inline>:1:14 in 'f'\n"
"\t<inline>:3:2 in <toplevel>"
,
"(define (f) (g))\n"
"(define (g) h)\n"
"(f)");
}

Test(schaf, runtime_error_symbol2) {
    expect_unbound_var_error(1, "x");
    expect_unbound_var_error(2, "(x)");
    expect_unbound_var_error(4, "(+ x)");
}

#define expect_unbound_var_error_syn(col, s, syn)   \
    expect_runtime_error( \
    "unbound variable: x\n" /* x: fixed variable name */  \
    "\t<inline>:1:" #col " in '" syn "'", s)

Test(schaf, runtime_error_syntax) {
    expect_runtime_error(
"unbound variable: x\n"
"\t<inline>:1:16 in 'f'\n"
"\t<inline>:2:2 in <toplevel>"
,
"(define (f) (+ x))\n"
"(f)");
    expect_unbound_var_error_syn(11, "(define y x)", "define");
    expect_unbound_var_error_syn(23, "(define y 42) (set! y x)", "set!");
    expect_unbound_var_error_syn(26, "(define y 42) (set! y (+ x))", "set!");
    expect_unbound_var_error_syn( 7, "(set! x 42)", "set!");
    expect_unbound_var_error_syn(14, "(begin (set! x 42) x)", "set!");
    expect_unbound_var_error_syn( 5, "(if x 1)", "if");
    expect_unbound_var_error_syn( 7, "(if 1 x)", "if");
    expect_unbound_var_error_syn(10, "(if #f 1 x)", "if");
    expect_unbound_var_error_syn( 8, "(cond (x 1))", "cond");
    expect_unbound_var_error_syn(10, "(cond (1 x))", "cond");
    expect_unbound_var_error_syn(13, "(cond (1 => x))", "cond");
    expect_unbound_var_error_syn(13, "(cond (else x))", "cond");
    expect_unbound_var_error_syn( 7, "(case x ((1) 2))", "case");
    expect_unbound_var_error_syn(14, "(case 1 ((1) x))", "case");
    expect_unbound_var_error_syn(15, "(case 1 (else x))", "case");
    expect_unbound_var_error_syn( 6, "(and x)", "and");
    expect_unbound_var_error_syn( 5, "(or x)", "or");
    expect_unbound_var_error_syn(10, "(let ((y x)) y)", "let");
    expect_unbound_var_error_syn(11, "(let* ((y x)) y)", "let*");
    expect_unbound_var_error_syn(15, "(let* ((y 1)) x)", "let*");
    expect_unbound_var_error_syn(13, "(letrec ((y x)) y)", "letrec");
    expect_unbound_var_error_syn(17, "(letrec ((y 1)) x)", "letrec");
    expect_unbound_var_error_syn( 8, "(begin x)", "begin");
    expect_unbound_var_error_syn(10, "(begin 1 x)", "begin");
    expect_unbound_var_error_syn( 9, "(do ((y x)) (1 y))", "do");
    expect_unbound_var_error_syn(14, "(do ((y 1)) (x 1))", "do");
    expect_unbound_var_error_syn(16, "(do ((y 1)) (1 x))", "do");
    expect_unbound_var_error_syn(27, "(do ((y 1 2)) ((> y 1) 2) x)", "do");
    expect_unbound_var_error_syn(11, "(do ((y 1 x)) ((> y 1) 2))", "do");
    expect_unbound_var_error_syn(21, "(case 1 ((1) (and 2 x)))", "and");
}

Test(schaf, runtime_error_syntax_knownbugs, .disabled = true) {
    expect_unbound_var_error( 4, "`(,x)");
    expect_runtime_error(
"unbound variable: x\n"
"\t<inline>:1:2 in 'quasiquote'\n"
"\t<inline>:1:1 in <toplevel>"
,
"`,`,x");
}

Test(schaf, div0) {
    expect_runtime_error("divided by zero", "(/ 42 0)");
}

Test(schaf, modulo) {
    expect_runtime_error("divided by zero", "(modulo 13 0)");
}

Test(schaf, unbound_variable) {
    expect_runtime_error("unbound variable: x", "x");
    expect_runtime_error("unbound variable: x", "(+ x 2)");
}

Test(schaf, if) {
    expect_runtime_error("2..3 but got 1", "(if #f)");
    expect_runtime_error("2..3 but got 4", "(if #f 1 2 3)");
}

Test(schaf, set) {
    expect_runtime_error("unbound variable: x", "(begin (set! x 42) x)");
}

Test(schaf, let) {
    expect_runtime_error("but got 1", "(let ((x 42)))");
    expect_runtime_error("but got 1", "(let ((x 42) (y 100)))");
}

Test(schaf, letrec) {
    expect_runtime_error("unbound variable: x", "(letrec ((y x)) 1)");
}

Test(schaf, applicable) {
    expect_runtime_error("expected procedure", "(1 1)");
    expect_runtime_error("expected procedure", "(() 1)");
}

Test(schaf, apply_invalid) {
    expect_runtime_error("expected null or pair", "(apply list 1)");
}

Test(schaf, map) {
    expect_runtime_error("expected null or pair but got integer", "(map + 1)");
    expect_runtime_error("expected null or pair but got integer", "(for-each + 1)");
}

Test(schaf, quasiquotes) {
    expect_runtime_error("unbound variable: x", "`(,x)");
}

Test(schaf, case) {
    expect_runtime_error("expected pair but got integer", "(case 1 (2 3))");
}

Test(table, get_put) {
    Table *t = table_new();
    table_put(t, 1, 100);
    cr_expect(eq(llong, 100, table_get(t, 1)));
    table_put(t, 2, 200);
    table_put(t, 3, 300);
    table_put(t, 4, 400);
    cr_expect(eq(llong, 100, table_get(t, 1)));
    cr_expect(eq(llong, 200, table_get(t, 2)));
    cr_expect(eq(llong, 300, table_get(t, 3)));
    cr_expect(eq(llong, 400, table_get(t, 4)));
    table_put(t, 1, 42);
    cr_expect(eq(llong, 42, table_get(t, 1)));
    cr_expect(eq(llong, 200, table_get(t, 2)));
    cr_expect(eq(llong, 300, table_get(t, 3)));
    cr_expect(eq(llong, 400, table_get(t, 4)));

    for (int i = 1; i <= 17; i++)
        table_put(t, i, i*10000000);
    for (int i = 1; i <= 17; i++)
        cr_expect(eq(llong, i*10000000, table_get(t, i)));

    table_free(t);
}

static void tabforeach(uint64_t k, uint64_t v, void *p)
{
    uint64_t *mul = p;
    *mul *= k * v;
}

Test(table, foreach) {
    Table *t = table_new();
    table_put(t, 2, 3);
    table_put(t, 5, 7);
    table_put(t, 11, 13);
    uint64_t mul = 1;
    table_foreach(t, tabforeach, &mul);

    cr_expect(eq(u64, 30030, mul));

    table_free(t);
}

Test(table, dup) {
    Table *t = table_new();
    for (long i = 1; i < 100; i++)
        table_put(t, i, i*17);
    Table *u = table_dup(t);
    table_free(t);

    cr_expect(not(eq(ptr, t, u)));
    for (long i = 1; i < 100; i++)
        cr_expect(eq(int, i*17, table_get(u, i)));

    table_free(u);
}

Test(table, string_keys) {
    Table *t = table_str_new();
    table_str_put(t, "foo", 12);
    table_str_put(t, "bar", 34);
    table_str_put(t, "foo", 56);

    cr_assert(eq(int, 56, table_str_get(t, "foo")));
    cr_assert(eq(int, 34, table_str_get(t, "bar")));

    table_free(t);
}
