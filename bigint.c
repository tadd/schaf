#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "bigint.h"
#include "libscary.h"
#include "utils.h"

struct BigInt {
    bool negative;
    uint32_t *digits; // use scary as little-endian
};

enum {
    RADIX = UINT32_C(0x1'0000'0000),
    NDIG10 = 9, // (floor (fllog10 UINT32_MAX))
    RADIX10 = UINT32_C(1'000'000'000), // (expt 10 NDIG10)
};
static const double RADIX_RATIO = 4.294967296; // (/ RADIX RADIX10)

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

static inline uint32_t raddiv(uint64_t x)
{
    return (uint32_t) (x / RADIX);
}

static inline uint32_t radmod(uint64_t x)
{
    return (uint32_t) (x % RADIX);
}

BigInt *bigint_from_int(int64_t x)
{
    BigInt *b = bigint_new();
    uint32_t upper;
    if (x >= 0) {
        b->negative = false;
        upper = raddiv(x);
    } else {
        b->negative = true;
        x = -(x + RADIX);
        upper = raddiv(x) + 1;
    }
    scary_push(&b->digits, radmod(x)); // lower
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

static int abs_eq(const uint32_t *x, const uint32_t *y)
{
    size_t len = scary_length(x);
    return len == scary_length(y) &&
        memcmp(x, y, sizeof(uint32_t) * len) == 0;
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
        scary_push(z, radmod(a));
        c = raddiv(a);
    }
    for (size_t i = ly; i < lx; i++) {
        uint64_t a = (uint64_t) x[i] + c;
        scary_push(z, radmod(a));
        c = raddiv(a);
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
    const uint32_t *dx = x->digits, *dy = y->digits;
    const uint32_t *dmax, *dmin;
    int cmp = abs_cmp(dx, dy);
    if (cmp > 0) {
        dmax = dx;
        dmin = dy;
    } else {
        dmax = dy;
        dmin = dx;
    }
    BigInt *z = bigint_new();
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

static void digits_ensure_length(uint32_t **d, size_t len)
{
    check_empty(*d);
    for (size_t i = 0; i < len; i++)
        scary_push(d, UINT32_C(0));
}

static void digits_pop_zeros(uint32_t *d)
{
    size_t len = scary_length(d);
    for (ssize_t i = len - 1; i > 0 && d[i] == 0; i--)
        scary_pop(d);
}

static bool is_zero(const uint32_t *d)
{
    return scary_length(d) == 1 && d[0] == 0;
}

static BigInt *normalize(BigInt *x)
{
    if (x->negative && is_zero(x->digits))
        x->negative = false;
    return x;
}

#define MAX(x, y) ((x) > (y) ? (x) : (y))
BigInt *bigint_mul(const BigInt *x, const BigInt *y)
{
    BigInt *z = bigint_new();
    z->negative = x->negative != y->negative;
    const uint32_t *dx = x->digits, *dy = y->digits;
    size_t lx = scary_length(dx), ly = scary_length(dy);
    digits_ensure_length(&z->digits, lx + ly);
    uint32_t *dz = z->digits, c = 0;
    for (size_t iy = 0; iy < ly; iy++) {
        uint32_t n = dy[iy];
        size_t iz = iy;
        uint64_t m;
        for (size_t ix = 0; ix < lx; ix++, iz++) {
            m = (uint64_t) dx[ix] * n + dz[iz] + c;
            dz[iz] = radmod(m);
            c = raddiv(m);
        }
        m = (uint64_t) dz[iz] + c;
        dz[iz] = radmod(m);
        c = raddiv(m);
    }
    dz[lx + ly - 1] += c;
    digits_pop_zeros(dz);
    return normalize(z);
}

UNUSED static void bigint_dump(const char *s, const uint32_t *a)
{
    size_t len = scary_length(a);
    fprintf(stderr, "%s: len=%zu", s, len);
    for (size_t i = 0; i < len; i++)
        fprintf(stderr, ", %u", a[i]);
    fprintf(stderr, "\n");
}

static uint32_t *convert_radix10(const uint32_t *src)
{
    size_t len = scary_length(src);
    uint32_t *tmp = scary_dup((void *) src);
    uint32_t *dst = scary_new(sizeof(uint32_t));
    size_t max_len = ceil(len * RADIX_RATIO);
    for (size_t i = 0; i < max_len && len > 0; i++) {
        uint32_t r = 0;
        for (ssize_t j = len - 1; j >= 0; j--) {
            uint64_t t = tmp[j] + r * RADIX;
            tmp[j] = t / RADIX10;
            r = t % RADIX10;
        }
        scary_push(&dst, r);
        while (len > 0 && tmp[len - 1] == 0)
            len--;
    }
    scary_free(tmp);
    return dst;
}

char *bigint_to_string(const BigInt *x)
{
    uint32_t *dig10 = convert_radix10(x->digits);
    size_t dlen = scary_length(dig10);
    size_t slen = NDIG10 * dlen + 2; // 2 for '-' and '\0'
    char *s = xmalloc(slen), *p = s;
    if (x->negative)
        *p++ = '-';
    ssize_t i = dlen - 1;
    p += sprintf(p, "%u", dig10[i]);
    while (--i >= 0)
        p += sprintf(p, "%.*u", (int) NDIG10, dig10[i]);
    scary_free(dig10);
    return s;
}
