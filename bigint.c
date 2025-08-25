#include <stdbool.h>
#include <stdint.h>

#include "libscary.h"
#include "utils.h"

typedef struct RawBigInt {
    bool negative;
    uint32_t *digits; // use scary as little-endian
} BigInt;

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
    scary_push(&b->digits, (uint32_t) (x >> 32U)); // upper
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

#if 0
bool bigint_gt(const BigInt *x, const BigInt *y)
{
    if (x->negative != y->negative)
        return x->negative ? false : true;
    int cmp = abs_cmp(x->digits, y->digits);
    return x->negative ? cmp < 0 : cmp > 0;
}
#endif

void scary_ensure_length(void **p, size_t len);
void scary_set_length(void **p, size_t len);

// assumes x > y
static void abs_add(uint32_t **z, const uint32_t *x, const uint32_t *y)
{
    size_t lx = scary_length(x), ly = scary_length(y);
    scary_ensure_length((void **) z, lx + 1);
    uint32_t c = 0;
    for (size_t i = 0; i < ly; i++) {
        uint64_t a = (uint64_t) x[i] + y[i] + c;
        *z[i] = (uint32_t) (a & 0xFFFF'FFFFU);
        c = a >> 32U;
    } 
    for (size_t i = ly; i < lx; i++) {
        uint64_t a = (uint64_t) x[i] + c;
        *z[i] = (uint32_t) (a & 0xFFFF'FFFFU);
        c = a >> 32U;
    }
    if (c > 0)
        *z[lx] = c;
    else
        scary_set_length((void **) z, lx); // decrease
}

// assumes x > y
static void abs_sub(uint32_t **z, const uint32_t *x, const uint32_t *y)
{
    size_t lx = scary_length(x), ly = scary_length(y);
    scary_ensure_length((void **) z, lx);
    uint32_t c = 0;
    for (size_t i = 0; i < ly; i++) {
        uint32_t max, min, lc;
        if (x[i] < y[i])
            max = y[i], min = x[i], lc = 1;
        else
            max = x[i], min = y[i], lc = 0;
        *z[i] = max - min - c;
        c = lc;
    } 
    for (size_t i = ly; i < lx; i++) {
        uint32_t max, min, lc;
        if (x[i] < c)
            max = c, min = x[i], lc = 1;
        else
            max = x[i], min = c, lc = 0;
        *z[i] = max - min;
        c = lc;
    }
}

void scary_clear(void *p);

static void set_zero(BigInt *x)
{
    x->negative = false;
    scary_clear(x->digits);
    scary_push(&x->digits, UINT32_C(0));
}

void bigint_add(BigInt *z, const BigInt *x, const BigInt *y)
{
    const uint32_t *dx, *dy;
    int cmp = abs_cmp(x->digits, y->digits);
    if (cmp > 0) {
        dx = x->digits;
        dy = y->digits;
    } else {
        dx = y->digits;
        dy = x->digits;
    }
    if (x->negative == y->negative) {
        z->negative = x->negative;
        abs_add(&z->digits, dx, dy);
        return;
    }
    if (cmp == 0) {
        set_zero(z);
        return;
    }
    z->negative = cmp > 0 ? x->negative : y->negative;
    abs_sub(&z->digits, dx, dy);
}
