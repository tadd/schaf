#include <stdbool.h>
#include <stdint.h>

#include "bigint.h"
#include "libscary.h"
#include "utils.h"

struct BigInt {
    bool negative;
    uint32_t *digits; // use scary as little-endian
};

static BigInt *bigint_new(void)
{
    BigInt *b = xmalloc(sizeof(BigInt));
    b->digits = scary_new(sizeof(uint32_t));
    return b;
}

void bigint_free(BigInt *x)
{
    if (x == NULL)
        return;
    scary_free(x->digits);
    free(x);
}

BigInt *bigint_from_int(int64_t x)
{
    BigInt *b = bigint_new();
    b->negative = x < 0;
    if (x < 0)
        x = -x;
    scary_push(&b->digits, (uint32_t) (x & 0xFFFF'FFFFU)); // lower
    uint32_t upper = x >> 32U;
    if (upper > 0)
        scary_push(&b->digits, upper);
    return b;
}

static int abs_cmp(const uint32_t *x, const uint32_t *y)
{
    size_t lx = scary_length(x), ly = scary_length(y);
    if (lx > ly)
        return 1;
    if (lx < ly)
        return -1;
    for (ssize_t i = lx - 1; i >= 0; i--) {
        if (x[i] > y[i])
            return 1;
        if (x[i] < y[i])
            return -1;
    }
    return 0;
}

static int abs_eq(const uint32_t *x, const uint32_t *y)
{
    size_t len = scary_length(x);
    if (len != scary_length(y))
        return false;
    for (size_t i = 0; i < len; i++) {
        if (x[i] != y[i])
            return false;
    }
    return true;
}

int bigint_cmp(const BigInt *x, const BigInt *y)
{
    if (x->negative != y->negative)
        return x->negative ? -1 : 1;
    int cmp = abs_cmp(x->digits, y->digits);
    return x->negative ? -cmp : cmp;
}

bool bigint_gt(const BigInt *x, const BigInt *y)
{
    return bigint_cmp(x, y) > 0;
}

bool bigint_ge(const BigInt *x, const BigInt *y)
{
    return bigint_cmp(x, y) >= 0;
}

bool bigint_lt(const BigInt *x, const BigInt *y)
{
    return bigint_cmp(x, y) < 0;
}

bool bigint_le(const BigInt *x, const BigInt *y)
{
    return bigint_cmp(x, y) <= 0;
}

bool bigint_eq(const BigInt *x, const BigInt *y)
{
    return x->negative == y->negative && abs_eq(x->digits, y->digits);
}

bool bigint_ne(const BigInt *x, const BigInt *y)
{
    return !bigint_eq(x, y);
}

#ifdef DEBUG
#define check_empty(a) do { \
        if (scary_length(a) > 0) \
            bug("length of digits was not 0"); \
    } while (0)
#else
#define check_empty(a) // nothing
#endif

// assumes x > y
static void abs_add(uint32_t **z, const uint32_t *x, const uint32_t *y)
{
    size_t lx = scary_length(x), ly = scary_length(y);
    uint32_t c = 0;
    for (size_t i = 0; i < ly; i++) {
        uint64_t a = (uint64_t) x[i] + y[i] + c;
        scary_push(z, (uint32_t) (a & 0xFFFF'FFFFU));
        c = a >> 32U;
    }
    for (size_t i = ly; i < lx; i++) {
        uint64_t a = (uint64_t) x[i] + c;
        scary_push(z, (uint32_t) (a & 0xFFFF'FFFFU));
        c = a >> 32U;
    }
    if (c > 0)
        scary_push(z, c);
}

// assumes x > y
static void abs_sub(uint32_t **z, const uint32_t *x, const uint32_t *y)
{
    size_t lx = scary_length(x), ly = scary_length(y);
    uint32_t c = 0;
    for (size_t i = 0; i < ly; i++) {
        uint32_t max, min, lc;
        if (x[i] < y[i])
            max = y[i], min = x[i], lc = 1;
        else
            max = x[i], min = y[i], lc = 0;
        scary_push(z, max - min - c);
        c = lc;
    }
    for (size_t i = ly; i < lx; i++) {
        uint32_t max, min, lc;
        if (x[i] < c)
            max = c, min = x[i], lc = 1;
        else
            max = x[i], min = c, lc = 0;
        scary_push(z, max - min);
        c = lc;
    }
}

static void set_zero(BigInt *x)
{
    x->negative = false;
    check_empty(x->digits);
    scary_push(&x->digits, UINT32_C(0));
}

static BigInt *add_or_sub(const BigInt *x, const BigInt *y, bool sub)
{
    BigInt *z = bigint_new();
    const uint32_t *dmax, *dmin;
    int cmp = abs_cmp(x->digits, y->digits);
    if (cmp > 0) {
        dmax = x->digits;
        dmin = y->digits;
    } else {
        dmax = y->digits;
        dmin = x->digits;
    }
    bool yneg = y->negative != sub;
    if (x->negative == yneg) {
        z->negative = x->negative;
        abs_add(&z->digits, dmax, dmin);
    } else if (cmp == 0)
        set_zero(z);
    else {
        z->negative = cmp > 0 ? x->negative : yneg;
        abs_sub(&z->digits, dmax, dmin);
    }
    return z;
}

BigInt *bigint_add(const BigInt *x, const BigInt *y)
{
    return add_or_sub(x, y, false);
}

BigInt *bigint_sub(const BigInt *x, const BigInt *y)
{
    return add_or_sub(x, y, true);
}
