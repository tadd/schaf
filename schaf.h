#ifndef SCHAF_H
#define SCHAF_H

#include <stdbool.h>
#include <stdint.h>

typedef uintptr_t SchValue;
typedef struct SchEngine SchEngine;

typedef enum {
    SCH_GC_ALGORITHM_MARK_SWEEP,
    SCH_GC_ALGORITHM_MARK_SWEEP_BITMAP,
    SCH_GC_ALGORITHM_EPSILON
} SchGCAlgorithm;

extern const SchValue SCH_NULL, SCH_UNDEF, SCH_FALSE, SCH_TRUE;

void sch_init_stack(const void *base);
#define SCH_INIT() uintptr_t sch_stack_base = 0; sch_init_stack(&sch_stack_base)
SchEngine *sch_new(void);
int sch_fin(SchEngine *e);

[[nodiscard, gnu::malloc, gnu::returns_nonnull]]
char *sch_stringify(SchValue v);
[[nodiscard, gnu::malloc, gnu::returns_nonnull]]
char *sch_inspect(SchValue v);
void sch_display(SchValue v);
SchValue sch_parse(const char *path);
SchValue sch_parse_string(const char *in);
SchValue sch_load(SchEngine *e, const char *path);
SchValue sch_eval_string(SchEngine *e, const char *s);
SchValue sch_eval_string_single(const char *s);

const char *sch_error_message(const SchEngine *e);

void sch_set_gc_init_size(SchEngine *e, double mib);
void sch_set_gc_stress(SchEngine *e, bool b);
void sch_set_gc_print_stat(SchEngine *e, bool b);
void sch_set_gc_algorithm(SchEngine *e, SchGCAlgorithm s);

#endif
