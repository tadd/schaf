#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <criterion/criterion.h>
#include <criterion/parameterized.h>
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

static uint64_t tabforeach_mul = 1;

static void tabforeach(uint64_t k, uint64_t v)
{
    tabforeach_mul *= k * v;
}

Test(table, foreach) {
    Table *t = table_new();
    table_put(t, 2, 3);
    table_put(t, 5, 7);
    table_put(t, 11, 13);
    table_foreach(t, tabforeach);

    cr_expect(eq(u64, 30030, tabforeach_mul));

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

// BigInt

static void test_bigint_init(void)
{
    srand(42);
}
TestSuite(bigint, .init = test_bigint_init);

typedef BigInt *BigIntPtr;
static char *cr_user_BigIntPtr_tostr(const BigIntPtr *x)
{
    return bigint_to_string(*x);
}
static int cr_user_BigIntPtr_eq(const BigIntPtr *x, const BigIntPtr *y)
{
    return bigint_eq(*x, *y);
}
static int cr_user_BigIntPtr_lt(const BigIntPtr *x, const BigIntPtr *y)
{
    return bigint_lt(*x, *y);
}

#define expect_bigint(op, x, y) cr_expect(op(type(BigIntPtr), x, y))
static void BigInt_clean(BigInt **x)
{
    bigint_free(*x);
}
#define autoptr(ty) __attribute__((cleanup(ty##_clean))) ty

Test(bigint, basic) {
    autoptr(BigInt) *x = bigint_from_int(INT64_MAX);
    autoptr(BigInt) *y = bigint_from_int(INT64_MIN);

    expect_bigint(gt, x, y);
    expect_bigint(ge, x, y);
    expect_bigint(lt, y, x);
    expect_bigint(le, y, x);

    expect_bigint(eq, x, x);
    expect_bigint(le, x, x);
    expect_bigint(ge, x, x);

    expect_bigint(eq, y, y);
    expect_bigint(le, y, y);
    expect_bigint(ge, y, y);

    autoptr(BigInt) *z = bigint_from_int(0);

    expect_bigint(eq, z, z);
    expect_bigint(gt, x, z);
    expect_bigint(lt, z, x);
    expect_bigint(gt, z, y);
    expect_bigint(lt, y, z);
}

Test(bigint, signs) {
    autoptr(BigInt) *a = bigint_from_int(1);
    autoptr(BigInt) *b = bigint_from_int(-1);
    autoptr(BigInt) *c = bigint_from_int(INT64_MAX);
    autoptr(BigInt) *d = bigint_from_int(INT64_MIN);
    autoptr(BigInt) *z = bigint_from_int(0);

    cr_expect(bigint_is_positive(a));
    cr_expect(not(bigint_is_negative(a)));
    cr_expect(bigint_is_negative(b));
    cr_expect(not(bigint_is_positive(b)));
    cr_expect(bigint_is_positive(c));
    cr_expect(not(bigint_is_negative(c)));
    cr_expect(bigint_is_negative(d));
    cr_expect(not(bigint_is_positive(d)));
    cr_expect(not(bigint_is_positive(z)));
    cr_expect(not(bigint_is_negative(z)));
}

Test(bigint, even_odd) {
    autoptr(BigInt) *a = bigint_from_int(1);
    autoptr(BigInt) *b = bigint_from_int(-1);
    autoptr(BigInt) *c = bigint_from_int(INT64_MAX);
    autoptr(BigInt) *d = bigint_from_int(INT64_MIN);
    autoptr(BigInt) *z = bigint_from_int(0);

    cr_expect(bigint_is_odd(a));
    cr_expect(bigint_is_odd(b));
    cr_expect(bigint_is_odd(c));
    cr_expect(not(bigint_is_odd(d)));
    cr_expect(not(bigint_is_odd(z)));
    cr_expect(not(bigint_is_even(a)));
    cr_expect(not(bigint_is_even(b)));
    cr_expect(not(bigint_is_even(c)));
    cr_expect(bigint_is_even(d));
    cr_expect(bigint_is_even(z));
}


Test(bigint, negate) {
    autoptr(BigInt) *x = bigint_from_int(INT64_MAX);
    autoptr(BigInt) *y = bigint_from_int(INT64_MIN+1);
    autoptr(BigInt) *z = bigint_from_int(0);
    autoptr(BigInt) *nx = bigint_from_int(-INT64_MAX);
    autoptr(BigInt) *ny = bigint_from_int(-(INT64_MIN+1));

    autoptr(BigInt) *nx2 = bigint_negate(x);
    autoptr(BigInt) *ny2 = bigint_negate(y);
    autoptr(BigInt) *nz2 = bigint_negate(z);

    expect_bigint(eq, nx, nx2);
    expect_bigint(eq, ny, ny2);
    expect_bigint(eq, z, nz2);

    autoptr(BigInt) *nx3 = bigint_negate(x);
    autoptr(BigInt) *ny3 = bigint_negate(y);
    autoptr(BigInt) *nz3 = bigint_negate(z);

    expect_bigint(eq, nx2, nx3);
    expect_bigint(eq, ny2, ny3);
    expect_bigint(eq, z, nz3);
}

#define C_OP(x) C_OP_##x
#define C_OP_add +
#define C_OP_sub -
#define C_OP_mul *
#define C_OP_div /
#define C_OP_mod %
#define expect_bigint_binop(op, ix, iy) do { \
        autoptr(BigInt) *exp = bigint_from_int((int64_t)(ix) C_OP(op) (iy)); \
        autoptr(BigInt) *x = bigint_from_int(ix); \
        autoptr(BigInt) *y = bigint_from_int(iy); \
        autoptr(BigInt) *z = bigint_##op(x, y); \
        expect_bigint(eq, exp, z); \
    } while (0)

Test(bigint, add) {
    expect_bigint_binop(add, INT64_MAX / 2, INT64_MAX / 2 - 10);
    expect_bigint_binop(add, INT64_MAX, INT64_MIN);
    expect_bigint_binop(add, INT64_MIN, INT64_MAX);
    expect_bigint_binop(add, INT64_MAX, -INT64_MAX);
    expect_bigint_binop(add, -INT64_MAX, INT64_MAX);
}

Test(bigint, sub) {
    expect_bigint_binop(sub, INT64_MAX, INT64_MAX);
    expect_bigint_binop(sub, 0, INT64_MIN + 1);
    expect_bigint_binop(sub, INT64_MAX, 0);
    expect_bigint_binop(sub, 0, INT64_MAX);
    expect_bigint_binop(sub, INT64_MAX, INT64_MAX);
}

Test(bigint, mul) {
    expect_bigint_binop(mul, INT32_MAX, INT32_MAX - 1);
    expect_bigint_binop(mul, INT32_MAX, INT32_MIN);
    expect_bigint_binop(mul, INT64_MAX, 0);
    expect_bigint_binop(mul, INT64_MIN, 0);
    expect_bigint_binop(mul, 0, INT64_MAX);
    expect_bigint_binop(mul, 0, INT64_MIN);
}

Test(bigint, div) {
    expect_bigint_binop(div, INT64_MAX, INT64_MAX / 3);
    expect_bigint_binop(div, 0, INT64_MAX);
    expect_bigint_binop(div, 3, INT64_MAX);
    expect_bigint_binop(div, -3, INT64_MAX);
    expect_bigint_binop(div, INT64_MAX, 3);
    expect_bigint_binop(div, INT64_MAX, -3);
}

Test(bigint, mod) {
    expect_bigint_binop(mod, INT64_MAX, INT64_MAX / 3);
    expect_bigint_binop(mod, 0, INT64_MAX);
    expect_bigint_binop(mod, 3, INT64_MAX);
    expect_bigint_binop(mod, -3, INT64_MAX);
    expect_bigint_binop(mod, INT64_MAX, 3);
    expect_bigint_binop(mod, INT64_MAX, -3);
}

Test(bigint, div_mod_divzero) {
    autoptr(BigInt) *x = bigint_from_int(1);
    autoptr(BigInt) *y = bigint_from_int(0);
    autoptr(BigInt) *div = bigint_div(x, y);
    autoptr(BigInt) *div2 = bigint_div(y, y);
    autoptr(BigInt) *mod = bigint_mod(x, y);
    autoptr(BigInt) *mod2 = bigint_mod(y, y);

    cr_expect(eq(ptr, div, NULL));
    cr_expect(eq(ptr, div2, NULL));
    cr_expect(eq(ptr, mod, NULL));
    cr_expect(eq(ptr, mod2, NULL));
}

Test(bigint, from_string) {
    autoptr(BigInt) *one = bigint_from_int(1);
    autoptr(BigInt) *none = bigint_from_int(-1);
    autoptr(BigInt) *max = bigint_from_int(INT64_MAX);
    autoptr(BigInt) *min = bigint_from_int(INT64_MIN+1);

    autoptr(BigInt) *a = bigint_from_string("0");
    cr_expect(not(bigint_is_negative(a)));
    cr_expect(not(bigint_is_positive(a)));

    autoptr(BigInt) *b = bigint_from_string("-0");
    cr_expect(not(bigint_is_negative(b)));
    cr_expect(not(bigint_is_positive(b)));

    autoptr(BigInt) *c = bigint_from_string("1");
    expect_bigint(eq, one, c);

    autoptr(BigInt) *d = bigint_from_string("-1");
    expect_bigint(eq, none, d);

    autoptr(BigInt) *e = bigint_from_string("9223372036854775807");
    expect_bigint(eq, max, e);

    autoptr(BigInt) *f = bigint_from_string("-9223372036854775807");
    expect_bigint(eq, min, f);

    autoptr(BigInt) *r1 = bigint_from_int(INT64_C(1) << 32U);
    autoptr(BigInt) *r2 = bigint_mul(r1, r1);
    autoptr(BigInt) *r4 = bigint_mul(r2, r2);
    autoptr(BigInt) *r8 = bigint_mul(r4, r4);
    autoptr(BigInt) *r16 = bigint_mul(r8, r8);
    autoptr(BigInt) *g = bigint_from_string("13407807929942597099574024"
    "998205846127479365820592393377723561443721764030073546976801874298"
    "166903427690031858186486050853753882811946569946433649006084096");
    expect_bigint(eq, r16, g);
}

#define expect_bigint_eq_from_strings(exp, suffix, str) do {       \
        autoptr(BigInt) *x = bigint_from_string_##suffix(str); \
        expect_bigint(eq, exp, x); \
    } while (0)

Test(bigint, from_string_nondecimal) {
    autoptr(BigInt) *zero = bigint_from_int(0);
    expect_bigint_eq_from_strings(zero, bin, "0");
    expect_bigint_eq_from_strings(zero, oct, "0");
    expect_bigint_eq_from_strings(zero, hex, "0");

    autoptr(BigInt) *one = bigint_from_int(1);
    expect_bigint_eq_from_strings(one, bin, "1");
    expect_bigint_eq_from_strings(one, oct, "1");
    expect_bigint_eq_from_strings(one, hex, "1");

    autoptr(BigInt) *none = bigint_from_int(-1);
    expect_bigint_eq_from_strings(none, bin, "-1");
    expect_bigint_eq_from_strings(none, oct, "-1");
    expect_bigint_eq_from_strings(none, hex, "-1");

    autoptr(BigInt) *max = bigint_from_int(INT64_MAX);
    expect_bigint_eq_from_strings(max, bin,
    "111111111111111111111111111111111111111111111111111111111111111");
    expect_bigint_eq_from_strings(max, oct, "777777777777777777777");
    expect_bigint_eq_from_strings(max, hex, "7fffffffffffffff");

    autoptr(BigInt) *min = bigint_from_int(INT64_MIN);
    expect_bigint_eq_from_strings(min, bin,
    "-1000000000000000000000000000000000000000000000000000000000000000");
    expect_bigint_eq_from_strings(min, oct, "-1000000000000000000000");
    expect_bigint_eq_from_strings(min, hex, "-8000000000000000");
}


struct BigInt { // copied for inspection
    bool negative;
    uint32_t *digits;
};

Test(bigint, mul_large) {
    int64_t i = (INT64_C(1) << 24U) + 1;
    uint32_t i2[] = { 0x2000001U, 0x10000U };
    uint32_t i3[] = { 0x3000001U, 0x30000U, 0x100U };
    uint32_t i4[] = { 0x4000001U, 0x60000U, 0x400U, 0x1U };

    autoptr(BigInt) *a = bigint_from_int(i);
    cr_expect(eq(u32, i, a->digits[0]));
    autoptr(BigInt) *a2 = bigint_mul(a, a);
    cr_expect_arr_eq(i2, a2->digits, sizeof(uint32_t)*2);
    autoptr(BigInt) *a3 = bigint_mul(a2, a);
    cr_expect_arr_eq(i3, a3->digits, sizeof(uint32_t)*3);
    autoptr(BigInt) *a3_2 = bigint_mul(a, a2);
    cr_expect_arr_eq(i3, a3_2->digits, sizeof(uint32_t)*3);
    expect_bigint(eq, a3, a3_2);

    autoptr(BigInt) *a4 = bigint_mul(a2, a2);
    cr_expect_arr_eq(i4, a4->digits, sizeof(uint32_t)*4);
    autoptr(BigInt) *a4_2 = bigint_mul(a3, a);
    cr_expect_arr_eq(i4, a4_2->digits, sizeof(uint32_t)*4);
    autoptr(BigInt) *a4_3 = bigint_mul(a, a3);
    cr_expect_arr_eq(i4, a4_3->digits, sizeof(uint32_t)*4);
    expect_bigint(eq, a4, a4_2);
    expect_bigint(eq, a4, a4_3);
    expect_bigint(eq, a4_2, a4_3);
}

#define expect_bigint_string_eq(exp, x) do { \
        char *s = bigint_to_string(x); \
        cr_assert(eq(str, exp, s)); \
        free(s); \
    } while (0)
#define expect_bigint_to_string(i) do { \
        autoptr(BigInt) *x = bigint_from_int(INT64_C(i)); \
        expect_bigint_string_eq(#i, x); \
    } while (0)

Test(bigint, to_string) {
    expect_bigint_to_string(0);
    expect_bigint_to_string(1);
    expect_bigint_to_string(42);
    expect_bigint_to_string(-1);
    expect_bigint_to_string(-42);
    expect_bigint_to_string(1000000000);
    expect_bigint_to_string(-1000000000);
    expect_bigint_to_string(12345678901);
    expect_bigint_to_string(-12345678901);
    expect_bigint_to_string(9223372036854775807); // MAX
    expect_bigint_to_string(-9223372036854775807);// MIN+1
}

Test(bigint, to_string_larger) {
    autoptr(BigInt) *a, *b, *c, *d;
    a = bigint_from_int(10000000000); // "0"*10
    expect_bigint_string_eq("1""0000000000", a);
    b = bigint_mul(a, a); // 20
    expect_bigint_string_eq("1""0000000000""0000000000", b);
    c = bigint_mul(b, a); // 30
    expect_bigint_string_eq("1""0000000000""0000000000""0000000000", c);
    d = bigint_mul(b, b); // 40
    expect_bigint_string_eq("1""0000000000""0000000000""0000000000""0000000000", d);

    autoptr(BigInt) *na, *nb, *nc, *nd;
    na = bigint_from_int(-10000000000); // -a
    expect_bigint_string_eq("-1""0000000000", na);
    nb = bigint_mul(a, na);
    expect_bigint_string_eq("-1""0000000000""0000000000", nb);
    nc = bigint_mul(b, na);
    expect_bigint_string_eq("-1""0000000000""0000000000""0000000000", nc);
    nd = bigint_mul(c, na);
    expect_bigint_string_eq("-1""0000000000""0000000000""0000000000""0000000000", nd);
}

#define expect_bigint_to_int(i) do { \
        autoptr(BigInt) *x = bigint_from_int(i); \
        cr_expect(eq(i64, i, bigint_to_int(x))); \
    } while (0)

Test(bigint, to_int) {
    expect_bigint_to_int(INT64_C(0));
    expect_bigint_to_int(INT64_C(1));
    expect_bigint_to_int(INT64_C(42));
    expect_bigint_to_int(INT64_C(-1));
    expect_bigint_to_int(INT64_C(-42));
    expect_bigint_to_int(INT64_C(1000000000));
    expect_bigint_to_int(INT64_C(-1000000000));
    expect_bigint_to_int(INT64_C(12345678901));
    expect_bigint_to_int(INT64_C(-12345678901));
    expect_bigint_to_int(INT64_MAX);
    expect_bigint_to_int(INT64_MIN+1);
}

static int64_t rand_n(unsigned n)
{
    const uint64_t max = ~UINT64_C(0) >> (64U - n);
    double drand = (double) rand() / RAND_MAX;
    uint64_t r = lround(drand * max);
    return r - max / 2 - 1;
}

enum {
    NRAND = 16
};

#define RandomIntPairParameters(n, s, t) \
static_assert(n > 0); \
static_assert(n <= 64); \
ParameterizedTestParameters(s, t) { \
    static int64_t pairs[2][NRAND]; \
    for (size_t i = 0; i < NRAND; i++) { \
        pairs[0][i] = rand_n(n); \
        pairs[1][i] = rand_n(n); \
    } \
    return cr_make_param_array(int64_t[2], pairs, NRAND); \
}

#define BigIntBinopRandomTest(n, op, expf) \
RandomIntPairParameters(n, bigint, op##_rand##n) \
ParameterizedTest(int64_t p[2], bigint, op##_rand##n) { \
    int64_t ix = p[0], iy = p[1]; \
    if (iy == 0) iy = 42; /* for div/mod */ \
    autoptr(BigInt) *x = bigint_from_int(ix), *y = bigint_from_int(iy); \
    autoptr(BigInt) *z1 = expf(op, ix, iy, x, y); \
    autoptr(BigInt) *z2 = bigint_##op(x, y); \
    expect_bigint(eq, z1, z2); \
}
#define C_OP_FUNC(op, ix, iy, x, y) bigint_from_int(((int64_t) ix) C_OP(op) (iy))
#define BigIntBinopRandomTestC(n, op) BigIntBinopRandomTest(n, op, C_OP_FUNC)
#define BFUNC(op, ix, iy, x, y) bigint_##op((y), (x))
#define BigIntBinopRandomTestSymRel(n, op) BigIntBinopRandomTest(n, op, BFUNC)

#define BigIntDivmodRandomTest(n) \
RandomIntPairParameters(n, bigint, divmod_rand##n) \
ParameterizedTest(int64_t p[2], bigint, divmod_rand##n) { \
    int64_t ix = p[0], iy = p[1]; \
    if (iy == 0) iy = 42; \
    autoptr(BigInt) *x = bigint_from_int(ix), *y = bigint_from_int(iy); \
    autoptr(BigInt) *div = bigint_div(x, y), *mod = bigint_mod(x, y); \
    autoptr(BigInt) *z1 = bigint_mul(y, div); \
    autoptr(BigInt) *z = bigint_add(z1, mod); \
    expect_bigint(eq, x, z); \
}

BigIntBinopRandomTestC(63, add)
BigIntBinopRandomTestC(63, sub)
BigIntBinopRandomTestC(32, mul)
BigIntBinopRandomTestC(64, div)
BigIntBinopRandomTestC(64, mod)

BigIntBinopRandomTestSymRel(64, add)
BigIntBinopRandomTestSymRel(64, mul)

BigIntDivmodRandomTest(64)
