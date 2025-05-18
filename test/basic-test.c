#include <stdlib.h>
#include <string.h>

#include <criterion/criterion.h>
#include <criterion/new/assert.h>

#include "schaf.h"

#define expect_stringify(exp, v) do { \
        char *s = stringify(v); \
        cr_expect_str_eq(s, exp); \
        free(s); \
    } while (0)

#define value_idfunc list
#define V(x) \
    _Generic(x, int: value_of_int, char *: value_of_string, Value: value_idfunc)(x)
#define expect_v_eq(expected, actual) do { \
        Value vexp = expected, vact = actual; \
        if (value_is_int(vexp)) \
            expect_int_eq(vexp, vact); \
        else if (value_is_string(vexp)) \
            expect_vstr_eq(value_to_string(vexp), vact); \
        else if (value_is_symbol(vexp)) \
            expect_vsym_eq(value_to_string(vexp), vact); \
    } while (0)
#define expect_list_eq(expected, actual) do { \
        Value exp = expected, act = actual; \
        cr_expect(value_is_pair(act)); \
        expect_int_eq(length(exp), length(act)); \
        for (; exp != Qnil; exp = cdr(exp), act = cdr(act)) \
            expect_v_eq(car(exp), car(act)); \
    } while (0)
#define expect_pair_eq(ecar, ecdr, act) do { \
        Value a = act; \
        cr_expect(value_is_pair(a)); \
        expect_v_eq(V(ecar), car(a)); \
        expect_v_eq(V(ecdr), cdr(a)); \
    } while (0)

#define expect_int_eq(exp, act) cr_expect(eq(int, exp, act))
#define expect_string_eq(exp, act) cr_expect_str_eq(act, exp)
#define expect_error(pattern, v) do { \
        expect_int_eq(Qundef, v); \
        char *m = strstr(error_message(), pattern); \
        cr_expect_not_null(m, "actual string:\n\"%s\"\ndid not include:\n\"%s\"", \
                           error_message(), pattern); \
    } while (0)

#define expect_no_error(v) \
    cr_expect_neq(v, Qundef, "got error with a message: '%s'", error_message())
#define expect_vx_eq(t, n, exp, act) do { \
        Value a = act; \
        expect_no_error(a); \
        expect_int_eq(t, value_type_of(a)); \
        expect_##n##_eq(exp, value_to_##n(a)); \
    } while (0)
#define expect_x_eq_parsed(x, exp, act) expect_##x##_eq(exp, parse_expr_string(act))

#define expect_vint_eq(exp, act) expect_vx_eq(TYPE_INT, int, exp, act)
#define expect_vstr_eq(exp, act) expect_vx_eq(TYPE_STRING, string, exp, act)
#define expect_vsym_eq(exp, act) expect_vx_eq(TYPE_SYMBOL, string, exp, act)

#define expect_vint_eq_parsed(exp, act) expect_x_eq_parsed(vint, exp, act)
#define expect_vstr_eq_parsed(exp, act) expect_x_eq_parsed(vstr, exp, act)
#define expect_vsym_eq_parsed(exp, act) expect_x_eq_parsed(vsym, exp, act)
#define expect_list_eq_parsed(exp, act) expect_x_eq_parsed(list, exp, act)
#define expect_pair_eq_parsed(ecar, ecdr, act) expect_pair_eq(ecar, ecdr, parse_expr_string(act))
#define expect_parse_error(exp, act) expect_error(exp, parse_expr_string(act))
#define expect_runtime_error(exp, act) expect_error(exp, eval_string(act))

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
    Value v = parse_string(in);
    if (v == Qundef || v == Qnil)
        return v;
    return car(v);
}

// terminate with Qundef
static Value list(Value arg, ...)
{
    if (arg == Qundef)
        return Qnil;
    Value vec[0x10] = { arg, }, o;
    va_list ap;
    va_start(ap, arg);
    ssize_t i;
    for (i = 1; (o = va_arg(ap, Value)) != Qundef; i++)
        vec[i] = o;
    va_end(ap);
    Value l = Qnil;
    while (--i >= 0)
        l = cons(vec[i], l);
    return l;
}

Test(schaf, printing) {
    expect_stringify("#t", Qtrue);
    expect_stringify("#f", Qfalse);
    expect_stringify("<undef>", Qundef);
    expect_stringify("()", Qnil);

    expect_stringify("0", value_of_int(0));
    expect_stringify("42", value_of_int(42));
    expect_stringify("-42", value_of_int(-42));

    expect_stringify("foo", value_of_symbol("foo"));

    expect_stringify("(1)", cons(V(1), Qnil));
    expect_stringify("(1 . 2)", cons(V(1), V(2)));
    expect_stringify("(1 2)", list(V(1), V(2), Qundef));
}

Test(schaf, parse_int) {
    expect_vint_eq_parsed(42, "42");
    expect_vint_eq_parsed(-42, "-42");
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
}

Test(schaf, parse_string_list) {
    expect_list_eq_parsed(list(V("abc"), V("def"), Qundef),
                          "(\"abc\" \"def\")");
}

#define caaaar(x) (car(car(car(car(x)))))
Test(schaf, cxr) {
    expect_vint_eq(42, caaaar(parse_expr_string("((((42))))")));
}

Test(schaf, parse_ident) {
    expect_vsym_eq_parsed("a", "a");
}

Test(schaf, parse_dot) {
    expect_pair_eq_parsed(1, 2, "(1 . 2)");
}

Test(schaf, parse_peculiar) {
    expect_vint_eq_parsed(42, "+42");
    cr_expect(value_is_symbol(parse_expr_string("+")));
}

Test(schaf, parse_lambda) {
    expect_list_eq_parsed(list(value_of_symbol("lambda"), Qnil, V(42), Qundef),
                          "(lambda () 42)");
}

Test(schaf, parse_broken) {
    expect_parse_error("got 'EOF'", "(");
    expect_parse_error("got 'EOF'", "'");
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

Test(schaf, applicable) {
    expect_runtime_error("expected procedure", "(1 1)");
    expect_runtime_error("expected procedure", "(() 1)");
}

Test(schaf, map) {
    expect_runtime_error("expected pair but got integer", "(map + 1)");
    expect_runtime_error("expected pair but got integer", "(for-each + 1)");
}

#define type_name(v) (char *) value_to_type_name(v)
Test(schaf, type_name) {
    cr_assert(eq(str, "boolean", type_name(Qfalse)));
    cr_assert(eq(str, "integer", type_name(V(42))));
    cr_assert(eq(str, "symbol", type_name(value_of_symbol("foo"))));
    cr_assert(eq(str, "undef", type_name(Qundef)));
    cr_assert(eq(str, "pair", type_name(cons(Qfalse, Qnil))));
    cr_assert(eq(str, "null", type_name(Qnil)));
    cr_assert(eq(str, "string", type_name(V("bar"))));
    // "procedure",
}

Test(table, get_put) {
    Table *t = table_new();
    table_put(t, 1, 100);
    cr_assert(eq(llong, 100, table_get(t, 1)));
    table_put(t, 2, 200);
    table_put(t, 3, 300);
    table_put(t, 4, 400);
    cr_assert(eq(llong, 100, table_get(t, 1)));
    cr_assert(eq(llong, 200, table_get(t, 2)));
    cr_assert(eq(llong, 300, table_get(t, 3)));
    cr_assert(eq(llong, 400, table_get(t, 4)));
    table_put(t, 1, 42);
    cr_assert(eq(llong, 42, table_get(t, 1)));
    cr_assert(eq(llong, 200, table_get(t, 2)));
    cr_assert(eq(llong, 300, table_get(t, 3)));
    cr_assert(eq(llong, 400, table_get(t, 4)));

    for (int i = 1; i <= 17; i++)
        table_put(t, i, i*10000000);
    for (int i = 1; i <= 17; i++)
        cr_assert(eq(llong, i*10000000, table_get(t, i)));

    table_free(t);
}

Test(table, inherit) {
    uint64_t N = TABLE_NOT_FOUND;
    Table *t = table_new();
    table_put(t, 1, 12);
    Table *u = table_inherit(t);
    table_put(u, 2, 34);

    cr_assert(eq(llong, 12, table_get(t, 1)));
    cr_assert(eq(llong,  N, table_get(t, 2)));

    cr_assert(eq(llong, 12, table_get(u, 1)));
    cr_assert(eq(llong, 34, table_get(u, 2)));

    table_free(u);
    table_free(t);
}

Test(table, inherit_nested) {
    uint64_t N = TABLE_NOT_FOUND;
    Table *t = table_new();
    table_put(t, 1, 2);
    Table *u = table_inherit(t);
    table_put(u, 2, 4);
    Table *v = table_inherit(u);
    table_put(v, 3, 6);

    cr_assert(eq(llong, 2, table_get(t, 1)));
    cr_assert(eq(llong, N, table_get(t, 2)));
    cr_assert(eq(llong, N, table_get(t, 3)));

    cr_assert(eq(llong, 2, table_get(u, 1)));
    cr_assert(eq(llong, 4, table_get(u, 2)));
    cr_assert(eq(llong, N, table_get(u, 3)));

    cr_assert(eq(llong, 2, table_get(v, 1)));
    cr_assert(eq(llong, 4, table_get(v, 2)));
    cr_assert(eq(llong, 6, table_get(v, 3)));

    table_free(v);
    table_free(u);
    table_free(t);
}

void tabforeach(uint64_t k, uint64_t v, void *data)
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

    cr_assert(eq(u64, 30030, l));

    table_free(t);
}
