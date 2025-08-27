#include <stdlib.h>
#include <string.h>

#include <criterion/criterion.h>
#include <criterion/new/assert.h>

#include "bigint.h"
#include "intern.h"
#include "schaf.h"

#define expect_stringify(exp, v) do { \
        char *s = sch_stringify(v); \
        cr_expect_str_eq(s, exp); \
        free(s); \
    } while (0)

#define value_idfunc list
#define V(x) \
    _Generic(x, int: sch_integer_new, char *: sch_string_new, Value: value_idfunc)(x)
#define expect_value_eq(expected, actual) do { \
        Value vexp = expected, vact = actual; \
        if (sch_value_is_integer(vexp)) \
            expect_int_eq(vexp, vact); \
        else if (sch_value_is_string(vexp)) \
            expect_vstr_eq(sch_string_to_cstr(vexp), vact); \
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
        Value a = act; \
        cr_expect(sch_value_is_pair(a)); \
        expect_value_eq(V(ecar), car(a)); \
        expect_value_eq(V(ecdr), cdr(a)); \
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
#define expect_runtime_error(exp, act) expect_error(exp, sch_eval_string(act))

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

#define expect_immutable_pair_error(s) \
    expect_runtime_error("cannot modify immutable pair", s)
Test(schaf, set_cxr) {
    expect_immutable_pair_error("(set-car! '(1) 2)");
    expect_immutable_pair_error("(set-cdr! '(1) '(2))");
    expect_immutable_pair_error("(set-car! `(1) 2)");
    expect_immutable_pair_error("(set-cdr! `(1) '(2))");
    expect_immutable_pair_error("(set-car! `(,1) 2)");
    expect_immutable_pair_error("(set-cdr! `(,1) '(2))");
    expect_immutable_pair_error("(set-car! `(,@'(1)) 2)");
    expect_immutable_pair_error("(set-cdr! `(,@'(1)) '(2))");
    expect_immutable_pair_error("(set-car! `(,@`(1)) 2)");
    expect_immutable_pair_error("(set-cdr! `(,@`(1)) '(2))");
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
"\t<inline>:1:14 in 'f'\n"
"\t<inline>:2:2 in <toplevel>"
,
"(define (f) (map g '()))\n"
"(f)");
}

Test(schaf, runtime_error_funcs) {
    expect_runtime_error(
"unbound variable: f\n"
"\t<inline>:1:9 in <toplevel>"
,
"(print (f))");
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
    expect_runtime_error("expected pair but got integer", "(map + 1)");
    expect_runtime_error("expected pair but got integer", "(for-each + 1)");
}

Test(schaf, quasiquotes) {
    expect_runtime_error("unbound variable: x", "`(,x)");
}

Test(schaf, quasiquotes_knownbugs, .disabled = true) {
    expect_runtime_error(
"unbound variable: x\n"
"\t<inline>:1:2 in 'quasiquote'\n"
"\t<inline>:1:1 in <toplevel>"
,
"`,`,x");
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

static void tabforeach(uint64_t k, uint64_t v, void *data)
{
    uint64_t *x = data;
    *x *= k * v;
}

Test(table, foreach) {
    Table *t = table_new();
    table_put(t, 2, 3);
    table_put(t, 5, 7);
    table_put(t, 11, 13);
    uint64_t l = 1;
    table_foreach(t, tabforeach, &l);

    cr_expect(eq(u64, 30030, l));

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

// Terminate arguments with NULL
static void bigint_free_all(BigInt *x, ...)
{
    bigint_free(x);
    va_list ap;
    va_start(ap, x);
    while ((x = va_arg(ap, BigInt *)) != NULL)
        bigint_free(x);
    va_end(ap);
}

Test(bigint, basic) {
    BigInt *x = bigint_from_int(300);
    BigInt *y = bigint_from_int(200);
    cr_assert(eq(int, 1, bigint_cmp(x, y)));

    BigInt *z = bigint_from_int(INT64_MAX);
    cr_assert(eq(int, 1, bigint_cmp(z, x)));
    cr_assert(eq(int, 1, bigint_cmp(z, y)));
    cr_assert(bigint_eq(z, z));

    BigInt *a = bigint_from_int(INT64_MIN);
    cr_assert(eq(int, -1, bigint_cmp(a, x)));
    cr_assert(eq(int, -1, bigint_cmp(a, y)));
    cr_assert(eq(int, -1, bigint_cmp(a, z)));
    cr_assert(bigint_eq(a, a));

    bigint_free_all(x, y, z, a, NULL);
}

Test(bigint, add) {
    int64_t ix = (INT64_C(1) << 32U) - 10;
    int64_t iy = (INT64_C(1) << 32U) - 20;
    int64_t iz = ix + iy;
    BigInt *x = bigint_from_int(ix);
    BigInt *y = bigint_from_int(iy);
    BigInt *exp = bigint_from_int(iz);
    BigInt *z = bigint_add(x, y);

    cr_assert(eq(int, true, bigint_eq(exp, z)));

    bigint_free_all(exp, x, y, z, NULL);
}

Test(bigint, add_negative) {
    int64_t ix = (INT64_C(1) << 32U) - 10;
    int64_t iy = -((INT64_C(1) << 32U) - 20);
    int64_t iz = ix + iy;
    BigInt *x = bigint_from_int(ix);
    BigInt *y = bigint_from_int(iy);
    BigInt *exp = bigint_from_int(iz);
    BigInt *z = bigint_add(x, y);

    cr_assert(eq(int, true, bigint_eq(exp, z)));

    bigint_free_all(exp, x, y, z, NULL);
}

Test(bigint, add_to_zero) {
    int64_t ix = (INT64_C(1) << 32U) - 10;
    int64_t iy = -ix;
    BigInt *x = bigint_from_int(ix);
    BigInt *y = bigint_from_int(iy);
    BigInt *exp = bigint_from_int(0);
    BigInt *z = bigint_add(x, y);

    cr_assert(eq(int, true, bigint_eq(exp, z)));

    bigint_free_all(exp, x, y, z, NULL);
}

Test(bigint, sub) {
    int64_t ix = (INT64_C(1) << 32U) - 10;
    int64_t iy = (INT64_C(1) << 32U) - 20;
    int64_t iz = ix - iy;
    BigInt *x = bigint_from_int(ix);
    BigInt *y = bigint_from_int(iy);
    BigInt *exp = bigint_from_int(iz);
    BigInt *z = bigint_sub(x, y);

    cr_assert(eq(int, true, bigint_eq(exp, z)));

    bigint_free_all(exp, x, y, z, NULL);
}

Test(bigint, sub_negative) {
    int64_t ix = (INT64_C(1) << 32U) - 10;
    int64_t iy = -((INT64_C(1) << 32U) - 20);
    int64_t iz = ix - iy;
    BigInt *x = bigint_from_int(ix);
    BigInt *y = bigint_from_int(iy);
    BigInt *exp = bigint_from_int(iz);
    BigInt *z = bigint_sub(x, y);

    cr_assert(eq(int, true, bigint_eq(exp, z)));

    bigint_free_all(exp, x, y, z, NULL);
}

Test(bigint, sub_to_zero) {
    int64_t i = (INT64_C(1) << 32U) - 10;
    BigInt *x = bigint_from_int(i);
    BigInt *y = bigint_from_int(i);
    BigInt *exp = bigint_from_int(0);
    BigInt *z = bigint_sub(x, y);

    cr_assert(eq(int, true, bigint_eq(exp, z)));

    bigint_free_all(exp, x, y, z, NULL);
}

Test(bigint, mul) {
    int64_t ix = (INT64_C(1) << 31U) - 10;
    int64_t iy = (INT64_C(1) << 31U) - 20;
    int64_t iz = ix * iy;
    BigInt *x = bigint_from_int(ix);
    BigInt *y = bigint_from_int(iy);
    BigInt *exp = bigint_from_int(iz);
    BigInt *z = bigint_mul(x, y);

    cr_assert(eq(int, true, bigint_eq(exp, z)));

    bigint_free_all(exp, x, y, z, NULL);
}

Test(bigint, mul_negative) {
    int64_t ix = (INT64_C(1) << 31U) - 10;
    int64_t iy = -((INT64_C(1) << 31U) - 20);
    int64_t iz = ix * iy;
    BigInt *x = bigint_from_int(ix);
    BigInt *y = bigint_from_int(iy);
    BigInt *exp = bigint_from_int(iz);
    BigInt *z = bigint_mul(x, y);

    cr_assert(eq(int, true, bigint_eq(exp, z)));

    bigint_free_all(exp, x, y, z, NULL);
}

Test(bigint, mul_to_zero) {
    int64_t ix = (INT64_C(1) << 31U) - 10;
    BigInt *x1 = bigint_from_int(ix);
    BigInt *x2 = bigint_from_int(-ix);
    BigInt *zero = bigint_from_int(0);
    BigInt *z1 = bigint_mul(x1, zero);
    BigInt *z2 = bigint_mul(x2, zero);
    BigInt *z3 = bigint_mul(zero, x1);
    BigInt *z4 = bigint_mul(zero, x2);

    cr_assert(eq(int, true, bigint_eq(zero, z1)));
    cr_assert(eq(int, true, bigint_eq(zero, z2)));
    cr_assert(eq(int, true, bigint_eq(zero, z3)));
    cr_assert(eq(int, true, bigint_eq(zero, z4)));

    bigint_free_all(zero, x1, x2, z1, z2, z3, z4, NULL);
}
