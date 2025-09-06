#include <stdbool.h>
#include <stdint.h>

#include "utils.h"
#define ATTR_MALLOC [[nodiscard, gnu::malloc]]

typedef struct BigInt BigInt;

ATTR_XMALLOC BigInt *bigint_from_int(int64_t x);
ATTR_MALLOC BigInt *bigint_from_string(const char *s); // NULL on invalid string
ATTR_MALLOC BigInt *bigint_from_string_bin(const char *s);
ATTR_MALLOC BigInt *bigint_from_string_oct(const char *s);
ATTR_MALLOC BigInt *bigint_from_string_hex(const char *s);
void bigint_free(BigInt *x);

ATTR_XMALLOC BigInt *bigint_negate(const BigInt *x); // -@
bool bigint_is_zero(const BigInt *x); // == 0
bool bigint_is_positive(const BigInt *x); // > 0
bool bigint_is_negative(const BigInt *x); // < 0

int bigint_cmp(const BigInt *x, const BigInt *y); // <=>
bool bigint_gt(const BigInt *x, const BigInt *y); // >
bool bigint_ge(const BigInt *x, const BigInt *y); // >=
bool bigint_lt(const BigInt *x, const BigInt *y); // <
bool bigint_le(const BigInt *x, const BigInt *y); // <=
bool bigint_eq(const BigInt *x, const BigInt *y); // ==
bool bigint_ne(const BigInt *x, const BigInt *y); // !=

ATTR_XMALLOC BigInt *bigint_add(const BigInt *x, const BigInt *y); // +
ATTR_XMALLOC BigInt *bigint_sub(const BigInt *x, const BigInt *y); // -
ATTR_XMALLOC BigInt *bigint_mul(const BigInt *x, const BigInt *y); // *
ATTR_MALLOC BigInt *bigint_div(const BigInt *x, const BigInt *y); // /, NULL on error
ATTR_MALLOC BigInt *bigint_mod(const BigInt *x, const BigInt *y); // %, NULL on error

ATTR_XMALLOC char *bigint_to_string(const BigInt *x);
int64_t bigint_to_int(const BigInt *x);
