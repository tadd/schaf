#include <stdbool.h>
#include <stdint.h>

typedef struct BigInt BigInt;

BigInt *bigint_from_int(int64_t x);
void bigint_free(BigInt *x);

int bigint_cmp(const BigInt *x, const BigInt *y); // <=>
bool bigint_gt(const BigInt *x, const BigInt *y); // >
bool bigint_ge(const BigInt *x, const BigInt *y); // >=
bool bigint_lt(const BigInt *x, const BigInt *y); // <
bool bigint_le(const BigInt *x, const BigInt *y); // <=
bool bigint_eq(const BigInt *x, const BigInt *y); // ==
bool bigint_ne(const BigInt *x, const BigInt *y); // !=

BigInt *bigint_add(const BigInt *x, const BigInt *y);
