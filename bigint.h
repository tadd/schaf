#ifndef BIGINT_H
#define BIGINT_H

#include <stdbool.h>
#include <stdint.h>

#include "utils.h"
#define ATTR_MALLOC [[nodiscard, gnu::malloc]]

typedef struct {
    bool negative;
    uint32_t *digits; // use scary as little-endian
} BigInt;

void bigint_init(BigInt *x);
void bigint_fin(BigInt *x);
void bigint_set(BigInt *y, const BigInt *x);
ATTR_XMALLOC BigInt *bigint_from_int(int64_t x);
ATTR_XMALLOC BigInt *bigint_dup(const BigInt *x);
ATTR_MALLOC BigInt *bigint_from_string(const char *s); // NULL on invalid string
ATTR_MALLOC BigInt *bigint_from_string_bin(const char *s);
ATTR_MALLOC BigInt *bigint_from_string_oct(const char *s);
ATTR_MALLOC BigInt *bigint_from_string_hex(const char *s);
void bigint_free(BigInt *x);

void bigint_negate(BigInt *y, const BigInt *x); // -@
bool bigint_is_positive(const BigInt *x); // > 0
bool bigint_is_negative(const BigInt *x); // < 0
bool bigint_is_even(const BigInt *x); // % 2 == 0
bool bigint_is_odd(const BigInt *x); // % 2 != 0

int bigint_cmp(const BigInt *x, const BigInt *y); // <=>
bool bigint_gt(const BigInt *x, const BigInt *y); // >
bool bigint_ge(const BigInt *x, const BigInt *y); // >=
bool bigint_lt(const BigInt *x, const BigInt *y); // <
bool bigint_le(const BigInt *x, const BigInt *y); // <=
bool bigint_eq(const BigInt *x, const BigInt *y); // ==
bool bigint_ne(const BigInt *x, const BigInt *y); // !=

void bigint_abs(BigInt *y, const BigInt *x); // |x|
void bigint_add(BigInt *z, const BigInt *x, const BigInt *y); // +
void bigint_sub(BigInt *z, const BigInt *x, const BigInt *y); // -
void bigint_mul(BigInt *z, const BigInt *x, const BigInt *y); // *
bool bigint_div(BigInt *z, const BigInt *x, const BigInt *y); // /, false on error
bool bigint_mod(BigInt *z, const BigInt *x, const BigInt *y); // %, false on error

ATTR_XMALLOC char *bigint_to_string(const BigInt *x);
int64_t bigint_to_int(const BigInt *x);

#endif
