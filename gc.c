#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "gc.h"
#include "utils.h"
#include "schaf.h"

//
// Runtime-locals (aka global variables)
//

static Value symbol_names; // ("name0" "name1" ...)

// Value (uintptr_t):
//   0b.....000 Pointer (Unchangeable pattern!)
//   0b.......1 Integer
//   0b......10 Symbol
//   0b0--00100 #f
//   0b0--01100 #t
//   0b0-010100 <undef>
typedef const uintptr_t Flag;
static Flag FLAG_NBIT_SYM = 2;
static Flag FLAG_NBIT_INT = 1;
static Flag FLAG_MASK     = 0b111; // for 64 bit machine
static Flag FLAG_MASK_SYM =  0b11;
static Flag FLAG_MASK_INT =   0b1;
static Flag FLAG_SYM      =  0b10;
static Flag FLAG_INT      =   0b1;
const Value Qfalse = 0b00100U;
const Value Qtrue  = 0b01100U;
const Value Qundef = 0b10100U; // may be an error or something

static bool is_immediate(Value v)
{
    return v & FLAG_MASK;
}

inline bool value_is_int(Value v)
{
    return v & FLAG_MASK_INT;
}

inline bool value_is_symbol(Value v)
{
    return (v & FLAG_MASK_SYM) == FLAG_SYM;
}

//
// value_is_*: Type Checks
//

static inline bool value_tag_is(Value v, ValueTag expected)
{
    return !is_immediate(v) && VALUE_TAG(v) == expected;
}

inline bool value_is_string(Value v)
{
    return value_tag_is(v, TAG_STR);
}

inline bool value_is_procedure(Value v)
{
    if (is_immediate(v))
        return false;
    switch (VALUE_TAG(v)) {
    case TAG_SYNTAX:
    case TAG_CFUNC:
    case TAG_CLOSURE:
    case TAG_CONTINUATION:
        return true;
    default:
        return false;
    }
}

inline bool value_is_pair(Value v)
{
    return value_tag_is(v, TAG_PAIR);
}

inline bool value_is_nil(Value v)
{
    return v == Qnil;
}

static Type immediate_type_of(Value v)
{
    if (value_is_int(v))
        return TYPE_INT;
    if (value_is_symbol(v))
        return TYPE_SYMBOL;
    if (v == Qtrue || v == Qfalse)
        return TYPE_BOOL;
    if (v == Qundef)
        return TYPE_UNDEF;
    UNREACHABLE();
}

Type value_type_of(Value v)
{
    if (is_immediate(v))
        return immediate_type_of(v);
    ValueTag t = VALUE_TAG(v);
    switch (t) {
    case TAG_STR:
    case TAG_PAIR:
        return (Type) t;
    case TAG_CFUNC:
    case TAG_SYNTAX:
    case TAG_CLOSURE:
    case TAG_CONTINUATION:
        return TYPE_PROC;
    }
    UNREACHABLE();
}

// value_to_*: Convert internal data to external plain C

inline int64_t value_to_int(Value x)
{
#if __x86_64__
    return (int64_t) x >> FLAG_NBIT_INT;
#else
    int64_t i = x;
    return (i - 1) / (1 << FLAG_NBIT_INT);
#endif
}

inline Symbol value_to_symbol(Value v)
{
    return (Symbol) v >> FLAG_NBIT_SYM;
}

static const char *name_nth(Value list, int64_t n)
{
    for (int64_t i = 0; i < n; i++) {
        list = cdr(list);
        if (list == Qnil)
            return NULL;
    }
    Value name = car(list);
    return STRING(name)->body;
}

static const char *unintern(Symbol sym)
{
    const char *name = name_nth(symbol_names, (int64_t) sym);
    if (name == NULL) // fatal; every known symbols should have a name
        error("symbol %lu not found", sym);
    return name;
}

inline const char *value_to_string(Value v)
{
    if (value_is_symbol(v))
        return unintern(value_to_symbol(v));
    return STRING(v)->body;
}

// value_of_*: Convert external plain C data to internal

inline Value value_of_int(int64_t i)
{
    Value v = i;
    return v << FLAG_NBIT_INT | FLAG_INT;
}

static inline Value list1(Value x)
{
    return cons(x, Qnil);
}

static Symbol intern(const char *name)
{
    Value last = Qnil;
    int64_t i = 0;
    // find
    for (Value p = symbol_names; p != Qnil; last = p, p = cdr(p)) {
        Value v = car(p);
        if (strcmp(STRING(v)->body, name) == 0)
            return i;
        i++;
    }
    // or put at `i`
    Value s = value_of_string(name);
    Value next = list1(s);
    if (last == Qnil)
        symbol_names = next;
    else
        PAIR(last)->cdr = next;
    return i;
}

inline Value value_of_symbol(const char *s)
{
    Symbol sym = intern(s);
    return (Value) (sym << FLAG_NBIT_SYM | FLAG_SYM);
}

//
// GC things
//

#define HEADER(v) ((Header *) v)

typedef struct {
    size_t size;
    bool allocated;
    bool living;
} Header;

typedef struct Chunk {
    Header h;
    struct Chunk *next;
} Chunk;

enum {
    ROOT_SIZE = 0x10,
    MiB = 1024 * 1024,
};
static size_t init_size = 13 * MiB;
static void *heap;
static Chunk *free_list;
static const Value *root[ROOT_SIZE];
static size_t nroot;
static bool print_stat;
static const volatile void *stack_base;

void gc_set_init_size(size_t init_mib)
{
    init_size = init_mib * MiB;
}

void gc_init(void)
{
    heap = malloc(init_size);
    if (heap == NULL)
        error("out of memory; initial malloc(%zu) failed", init_size);
    Chunk *ch = heap;
    ch->h.size = init_size - sizeof(Header);
    ch->h.allocated = false;
    ch->h.living = false;
    ch->next = NULL;
    free_list = ch;

    symbol_names = Qnil;
    gc_add_root(&symbol_names);
}

static void *allocate_from_list(Chunk *prev, Chunk *curr, size_t size)
{
    uint8_t *p = (uint8_t *) curr;
    size_t hsize = size + sizeof(Header);
    Chunk *next = curr->next;
    if (curr->h.size > hsize) {
        Header h = curr->h;
        h.size -= hsize;
        Chunk *ch = (Chunk *)(p + hsize);
        ch->h = h;
        ch->next = next;
        next = ch;
    }
    if (prev == NULL)
        free_list = next;
    else
        prev->next = next;
    Header *o = HEADER(p);
    o->size = size;
    o->allocated = true;
    return o + 1; // user of allocation use curr->next space and so-on
}

static inline size_t align(size_t size)
{
    return (size + 7U) / 8U * 8U;
}

static void *allocate(size_t size)
{
    size = align(size);
    size_t hsize = size + sizeof(Header);
    for (Chunk *prev = NULL, *curr = free_list; curr != NULL; prev = curr, curr = curr->next) {
        if (curr->h.size >= hsize) // First-fit
            return allocate_from_list(prev, curr, size);
    }
    return NULL;
}

void gc_stack_init(const volatile void *b)
{
    stack_base = b;
}

size_t gc_stack_get_size(const volatile void *sp)
{
    return stack_base - sp;
}

void gc_add_root(const Value *r)
{
    if (nroot == ROOT_SIZE)
        error("%s: too many roots added", __func__);
    root[nroot++] = r;
}

static inline Header *get_header(Value v)
{
    return HEADER(v) - 1;
}

static void mark(Value v)
{
    if (is_immediate(v) || v == Qnil)
        return;
    Header *h = get_header(v);
    if (h->living)
        return;
    h->living = true;
    switch (VALUE_TAG(v)) {
    case TAG_PAIR: {
        Pair *p = PAIR(v);
        mark(p->car);
        mark(p->cdr);
        return;
    }
    case TAG_CLOSURE: {
        Closure *p = CLOSURE(v);
        mark(p->env);
        mark(p->params);
        mark(p->body);
        return;
    }
    case TAG_CONTINUATION: {
        Continuation *p = CONTINUATION(v);
        mark(p->call_stack);
        mark(p->retval);
        return;
    }
    case TAG_STR:
    case TAG_CFUNC:
    case TAG_SYNTAX:
        return;
    }
}

static void mark_roots(void)
{
    for (size_t i = 0; i < nroot; i++)
        mark(*root[i]);
}

static bool in_heap_range(uintptr_t v)
{
    if (v < sizeof(Header)) // cannot get Header*
        return false;
    const void *p = get_header(v);
    return p >= heap && p < heap + init_size;
}

static bool in_heap(uintptr_t v)
{
    if (is_immediate(v) || v == Qnil ||
        v % 8U != 0 || !in_heap_range(v))
        return false;
    ValueTag t = VALUE_TAG((Value) v);
    return t >= TAG_PAIR && t <= TAG_SYNTAX; // need to be precise more?
}

static void mark_maybe(uintptr_t v)
{
    if (in_heap(v)) {
        //debug("marking: %zx", v);
        mark((Value) v);
    }
}

#define GET_SP(p) volatile void *p = &p

ATTR(noinline)
static void mark_stack(void)
{
    GET_SP(sp);
    uintptr_t *beg = (uintptr_t *) sp, *end = (uintptr_t *) stack_base;
    for (uintptr_t *p = beg; p < end; p++)
        mark_maybe(*p);
}

ATTR(unused)
static void heap_dump(void)
{
    uint8_t *p = heap, *endp = p + init_size;
    fprintf(stderr, "begin: %p..%p\n", p, endp);
    size_t offset;
    bool ellipsis = false;
    for (Header *h, *prev = NULL; p < endp; p += offset, prev = h) {
        h = HEADER(p);
        offset = h->size + sizeof(Header);
        if (prev != NULL && h->size == prev->size &&
            h->allocated == prev->allocated && h->living == prev->living) {
            if (!ellipsis) {
                fprintf(stderr, "  [..]\n");
                ellipsis = true;
            }
            continue;
        }
        ellipsis = false;
        fprintf(stderr, "  [%p] size: %zu, alloc: %d, liv: %d\n",
                h, h->size, h->allocated, h->living);
    }
    fprintf(stderr, "end: %p..%p\n", p, endp);
}

#define TABMAX 1024

ATTR(unused)
static void heap_stat_table(size_t tab[])
{
    for (size_t i = 0; i <= TABMAX; i++) {
        if (tab[i] > 0)
            fprintf(stderr, "    [%zu] %zu\n", i, tab[i]);
    }
}

static void heap_stat(const char *header)
{
    if (header != NULL)
        debug("%s", header);
    uint8_t *p = heap, *endp = p + init_size;
    size_t offset;
    size_t used = 0;
    size_t tab_used[TABMAX+1] = { 0, }, tab_free[TABMAX+1] = { 0, };
    for (Header *h; p < endp; p += offset) {
        h = HEADER(p);
        offset = h->size + sizeof(Header);
        size_t i = h->size > TABMAX ? TABMAX : h->size;
        if (h->allocated) {
            used += offset;
            tab_used[i]++;
        } else
            tab_free[i]++;
    }
    int n = ceil(log10(init_size));
    long r = lround(((double) used / init_size) * 1000);
    debug("  heap usage: %*zu / %*zu (%3ld.%1ld%%)",
          n, used, n, init_size, r/10, r%10);
    debug("  used dist:");
    heap_stat_table(tab_used);
    debug("  free dist:");
    heap_stat_table(tab_free);
}

static void add_to_free_list(void *p)
{
    Chunk *ch = p;
    ch->next = free_list; // prepend
    free_list = ch;
}

static void sweep(void)
{
    static Header dummy = { .size = 0, .allocated = true, .living = false };
    uint8_t *p = heap, *endp = p + init_size;
    size_t offset;
    for (Header *h, *prev = &dummy; p < endp; p += offset) {
        h = HEADER(p);
        offset = h->size + sizeof(Header);
        if (h->living) {
            h->living = false;
            prev = h;
        } else if (h->allocated) {
            if (!prev->allocated)
                prev->size += offset;
            else {
                add_to_free_list(h);
                h->allocated = false;
                prev = h;
            }
        }
    }
}

void gc_set_print_stat(bool b)
{
    print_stat = b;
}

static void mark_all(void)
{
    mark_roots();
    jmp_buf jmp;
    memset(&jmp, 0, sizeof(jmp_buf));
    setjmp(jmp);
    mark_stack(); // mark register values in jmp too
}

static void gc(void)
{
    if (print_stat)
        heap_stat("GC begin");
    mark_all();
    sweep();
    if (print_stat)
        heap_stat("GC end");
}

void *xmalloc(size_t size)
{
    void *p = allocate(size);
    if (p == NULL) {
        gc();
        p = allocate(size);
    }
    if (p == NULL)
        error("out of memory; %s(%zu) failed", __func__, size);
    return p;
}
