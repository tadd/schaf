#include <ctype.h>
#include <inttypes.h>
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
    RADIX = 0x1'0000'0000U,
    NDIG2 = 32,
    NDIG8 = 10, // (floor (/ NDIG2 (fllog2 8)))
    NDIG10 = 9, // (floor (fllog10 RADIX))
    NDIG16 = 8, // (floor (/ NDIG2 (fllog2 16)))
    RADIX8 =  1'073'741'824U, // (expt 8 NDIG8)
    RADIX10 = 1'000'000'000U, // (expt 10 NDIG10)
};

#define digits_new() scary_new(sizeof(uint32_t))
#define digits_new_sized(n) scary_new_sized((n), sizeof(uint32_t))
// digits are zero-cleared
static BigInt *bigint_new_sized(size_t n)
{
    BigInt *b = xmalloc(sizeof(BigInt));
    b->digits = digits_new_sized(n);
    return b;
}

static BigInt *bigint_new(void)
{
    BigInt *b = xmalloc(sizeof(BigInt));
    b->digits = digits_new();
    return b;
}

void bigint_free(BigInt *x)
{
    if (x == NULL)
        return;
    scary_free(x->digits);
    free(x);
}

static BigInt* bigdup(const BigInt *x)
{
    BigInt *y = bigint_new();
    y->negative = x->negative;
    y->digits = scary_dup(x->digits);
    return y;
}

static bool is_zero(const uint32_t *d)
{
    return scary_length(d) == 1 && d[0] == 0;
}

bool bigint_is_zero(const BigInt *x)
{
    return is_zero(x->digits);
}

bool bigint_is_positive(const BigInt *x)
{
    return !x->negative && !is_zero(x->digits);
}

bool bigint_is_negative(const BigInt *x)
{
    return x->negative;
}

BigInt *bigint_negate(const BigInt *x)
{
    BigInt *y = bigdup(x);
    if (!is_zero(y->digits))
        y->negative = !y->negative;
    return y;
}

static uint32_t scan_int(const char *s, ssize_t n, unsigned radix)
{
    char buf[n + 1];
    strncpy(buf, s, n);
    buf[n] = '\0';
    return strtoul(buf, NULL, radix);
}

static uint32_t *convert_radix(const uint32_t *src, size_t rad_src, size_t rad_dst)
{
    size_t len = scary_length(src);
    uint32_t *tmp = scary_dup((void *) src);
    uint32_t *dst = digits_new();
    size_t max_len = ceil(len * log(rad_src) / log(rad_dst));
    for (size_t i = 0; i < max_len && len > 0; i++) {
        uint32_t r = 0;
        for (ssize_t j = len - 1; j >= 0; j--) {
            uint64_t t = tmp[j] + r * rad_src;
            tmp[j] = t / rad_dst;
            r = t % rad_dst;
        }
        scary_push(&dst, r);
        while (len > 0 && tmp[len - 1] == 0)
            len--;
    }
    scary_free(tmp);
    return dst;
}

static bool digits_pop_zeros(uint32_t *d)
{
    size_t len = scary_length(d);
    bool ret = false;
    for (ssize_t i = len - 1; i > 0 && d[i] == 0; i--) {
        scary_pop(d);
        ret = true;
    }
    return ret;
}

static BigInt *normalize(BigInt *x)
{
    digits_pop_zeros(x->digits);
    if (x->negative && is_zero(x->digits))
        x->negative = false;
    return x;
}

static BigInt *from_string(const char *s, unsigned radix,
                           size_t radix_ndig,
                           uint64_t inner_radix, // 0 means no conversion needed
                           int (*is_acceptable)(int c))
{
    const char *p = s;
    bool negative = false;
    while (isspace(*p))
        p++;
    if (*p == '+')
        p++;
    else if (*p == '-') {
        negative = true;
        p++;
    }
    const char *dbeg = p;
    while (is_acceptable(*p))
        p++;
    size_t len = p - dbeg;
    if (len == 0)
        return NULL;
    size_t div = len / radix_ndig, mod = len % radix_ndig;
    size_t ndig = div + (mod > 0 ? 1 : 0);
    BigInt *b = bigint_new_sized(ndig);
    b->negative = negative;
    uint32_t *d = b->digits;
    if (mod > 0) {
        d[ndig-1] = scan_int(dbeg, mod, radix);
        ndig--;
    }
    p = dbeg + mod;
    for (ssize_t i = ndig - 1; i >= 0; i--) {
        d[i] = scan_int(p, radix_ndig, radix);
        p += radix_ndig;
    }
    if (inner_radix > 0) {
        b->digits = convert_radix(d, inner_radix, RADIX);
        scary_free(d);
    }
    return normalize(b);
}

static int is_binary(int c)
{
    return c == '0' || c == '1';
}

static int is_octal(int c)
{
    return c >= '0' && c <= '7';
}

BigInt *bigint_from_string(const char *s)
{
    return from_string(s, 10, NDIG10, RADIX10, isdigit);
}

BigInt *bigint_from_string_bin(const char *s)
{
    return from_string(s, 2, NDIG2, 0, is_binary);
}

BigInt *bigint_from_string_oct(const char *s)
{
    return from_string(s, 8, NDIG8, RADIX8, is_octal);
}

BigInt *bigint_from_string_hex(const char *s)
{
    return from_string(s, 16, NDIG16, 0, isxdigit);
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

// assumes x > y
static void abs_add(uint32_t **z, const uint32_t *x, const uint32_t *y)
{
    size_t i, lx = scary_length(x), ly = scary_length(y);
    uint32_t c = 0;
    for (i = 0; i < ly; i++) {
        uint64_t a = (uint64_t) x[i] + y[i] + c;
        scary_push(z, radmod(a));
        c = raddiv(a);
    }
    for (; i < lx; i++) {
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
    size_t i, lx = scary_length(x), ly = scary_length(y);
    uint32_t c = 0;
    for (i = 0; i < ly; i++) {
        uint32_t xi = x[i], yi = y[i], lc = 0;
        if (xi < yi)
            xi += RADIX, lc = 1;
        scary_push(z, xi - yi - c);
        c = lc;
    }
    for (; i < lx; i++) {
        uint32_t xi = x[i], lc = 0;
        if (xi < c)
            xi += RADIX, lc = 1;
        scary_push(z, xi - c);
        c = lc;
    }
    digits_pop_zeros(*z);
}

static void set_zero(BigInt *x)
{
    x->negative = false;
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

static void abs_mul_int(uint32_t *y, const uint32_t *x, uint32_t n, size_t xlen)
{
    uint32_t c = 0;
    uint64_t m;
    for (size_t i = 0; i < xlen; i++) {
        m = (uint64_t) x[i] * n + y[i] + c;
        y[i] = radmod(m);
        c = raddiv(m);
    }
    y[xlen] += c;
}

#define MAX(x, y) ((x) > (y) ? (x) : (y))
BigInt *bigint_mul(const BigInt *x, const BigInt *y)
{
    const uint32_t *dx = x->digits, *dy = y->digits;
    size_t lx = scary_length(dx), ly = scary_length(dy);
    BigInt *z = bigint_new_sized(lx + ly);
    z->negative = x->negative != y->negative;
    uint32_t *dz = z->digits;
    for (size_t i = 0; i < ly; i++)
        abs_mul_int(dz + i, dx, dy[i], lx);
    return normalize(z);
}

static uint64_t msb_to_u64(const uint32_t *x, size_t n)
{
    size_t i = scary_length(x) - 1;
    if (n == 1)
        return x[i];
    return ((uint64_t) x[i] << 32U) | x[i-1];
}

static int check_div(const uint32_t *x, const uint32_t *y, uint32_t div,
                     uint32_t **buf, size_t ylen)
{
    abs_mul_int(*buf, y, div, ylen);
    bool poped = digits_pop_zeros(*buf);
    if (abs_cmp(*buf, x) > 0) {
        if (poped)
            scary_push(buf, UINT32_C(0));
        return 1;
    }
    uint32_t *mod = digits_new();
    abs_sub(&mod, x, *buf);
    int cmp = abs_cmp(mod, y);
    scary_free(mod);
    return cmp > 0 ? -1 : 0;
}

static uint32_t calc_div(const uint32_t *x, const uint32_t *y, uint32_t init)
{
    uint64_t div = init, upper = RADIX, lower = 0;
    size_t len = scary_length(y);
    uint32_t *buf = digits_new_sized(len + 1);
    int c;
    while ((c = check_div(x, y, div, &buf, len)) != 0) {
        if (c > 0) {
            upper = div - 1;
            div = (div + lower) / 2;
        } else {
            lower = div + 1;
            div = (div + upper) / 2 + 1;
        }
    }
    scary_free(buf);
    return div;
}

#define SET_PTR(ptr, val) do { \
        if (ptr != NULL) \
            *ptr = (val); \
    } while (0)

static uint32_t abs_divmod_single(const uint32_t *x, const uint32_t *y,
                                  uint32_t **pmod)
{
    size_t lx = scary_length(x), ly = scary_length(y);
    uint32_t div;
    if (lx == 1) {
        div = x[0] / y[0];
        goto out;
    }
    size_t ny = lx > ly ? 1 : 2;
    uint64_t x2 = msb_to_u64(x, 2), y2 = msb_to_u64(y, ny);
    div = x2 / y2;
    if (lx == 2)
        goto out;
    div = calc_div(x, y, div);
 out:
    uint32_t *mul = digits_new_sized(ly + 1);
    abs_mul_int(mul, y, div, ly);
    digits_pop_zeros(mul);
    uint32_t *mod = digits_new();
    abs_sub(&mod, x, mul);
    scary_free(mul);
    *pmod = mod;
    return div;
}

static uint32_t *digits_rshift(const uint32_t *x, size_t n)
{
    uint32_t *y = digits_new();
    for (ssize_t i = n, len = scary_length(x); i < len; i++)
        scary_push(&y, x[i]);
    return y;
}

static uint32_t *lshift1_add(const uint32_t *x, uint32_t a)
{
    if (is_zero(x)) {
        uint32_t *y = digits_new();
        scary_push(&y, a);
        return y;
    }
    size_t len = scary_length(x);
    if (x[len-1] == 0)
        UNREACHABLE();
    uint32_t *y = digits_new_sized(len + 1);
    y[0] = a;
    memcpy(y+1, x, sizeof(uint32_t) * len);
    return y;
}

// assumes x > y
static void abs_divmod(const uint32_t *x, const uint32_t *y,
                       BigInt **pdiv, BigInt **pmod)
{
    size_t lx = scary_length(x), ly = scary_length(y);
    size_t n = lx - ly;
    BigInt *div = bigint_new_sized(n + 1);
    uint32_t *ddiv = div->digits, *dmod = NULL;
    uint32_t *x2 = digits_rshift(x, n);
    for (ssize_t i = n; i >= 0; i--) {
        ddiv[i] = abs_divmod_single(x2, y, &dmod);
        scary_free(x2);
        if (i > 0) {
            x2 = lshift1_add(dmod, x[i-1]);
            scary_free(dmod);
        }
    }
    digits_pop_zeros(ddiv);
    if (pdiv != NULL)
        *pdiv = div;
    else
        bigint_free(div);
    if (pmod != NULL) {
        BigInt *mod = bigint_new();
        scary_free(mod->digits);
        mod->digits = dmod;
        *pmod = mod;
    } else
        scary_free(dmod);
}

static void divmod(const BigInt *x, const BigInt *y, BigInt **pdiv, BigInt **pmod)
{
    const uint32_t *dx = x->digits, *dy = y->digits;
    if (is_zero(dy)) { // div zero!
        SET_PTR(pdiv, NULL);
        SET_PTR(pmod, NULL);
        return;
    }
    int cmp = abs_cmp(dx, dy);
    bool neg = x->negative != y->negative;
    if (cmp < 0) {
        SET_PTR(pdiv, bigint_from_int(0));
        SET_PTR(pmod, bigdup(x));
    } else if (cmp == 0) {
        SET_PTR(pdiv, bigint_from_int(neg ? -1 : 1));
        SET_PTR(pmod, bigint_from_int(0));
    } else {
        abs_divmod(dx, dy, pdiv, pmod);
        if (pdiv != NULL)
            (*pdiv)->negative = neg;
        if (pmod != NULL)
            (*pmod)->negative = x->negative;
    }
}

BigInt *bigint_div(const BigInt *x, const BigInt *y)
{
    BigInt *div;
    divmod(x, y, &div, NULL);
    return div;
}

BigInt *bigint_mod(const BigInt *x, const BigInt *y)
{
    BigInt *mod;
    divmod(x, y, NULL, &mod);
    return mod;
}

UNUSED static void bigint_dump(const char *s, const uint32_t *a)
{
    size_t len = scary_length(a);
    fprintf(stderr, "%s: len=%zu", s, len);
    for (size_t i = 0; i < len; i++)
        fprintf(stderr, ", %u", a[i]);
    fprintf(stderr, "\n");
}

char *bigint_to_string(const BigInt *x)
{
    uint32_t *dig10 = convert_radix(x->digits, RADIX, RADIX10);
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
