#ifndef LISP_H
#define LISP_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "utils.h"

typedef struct Pair Pair;
typedef uintptr_t Value;

extern const Value Qnil;

bool value_is_int(Value v);
bool value_is_symbol(Value v ATTR_UNUSED);
bool value_is_atom(Value v);
bool value_is_string(Value v);
bool value_is_pair(Value v);
bool value_is_nil(Value v);

Value value_of_int(int64_t i);
Value value_of_string(const char *s);

int64_t value_to_int(Value v);
const char *value_to_string(Value v);

Value cons(Value car, Value cdr);
Value car(Value v);
Value cdr(Value v);
// 2
Value caar(Value v);
Value cadr(Value v);
Value cdar(Value v);
Value cddr(Value v);
// 3
Value caaar(Value v);
Value caadr(Value v);
Value cadar(Value v);
Value caddr(Value v);
Value cdaar(Value v);
Value cdadr(Value v);
Value cddar(Value v);
Value cdddr(Value v);
// 4
Value caaaar(Value v);
Value caaadr(Value v);
Value caadar(Value v);
Value caaddr(Value v);
Value cadaar(Value v);
Value cadadr(Value v);
Value caddar(Value v);
Value cadddr(Value v);
Value cdaaar(Value v);
Value cdaadr(Value v);
Value cdadar(Value v);
Value cdaddr(Value v);
Value cddaar(Value v);
Value cddadr(Value v);
Value cdddar(Value v);
Value cddddr(Value v);

void print(Value v);
Value parse(FILE *in);
Value eval(Value v);
ATTR_MALLOC char *stringify(Value v);
Value parse_expr_from_string(const char *in);

#endif
