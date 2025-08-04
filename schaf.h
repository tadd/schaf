#ifndef SCHAF_H
#define SCHAF_H

#include <stdbool.h>
#include <stdint.h>

typedef uintptr_t SchValue;

typedef enum : int {
    GC_STRATEGY_MARK_SWEEP,
    GC_STRATEGY_EPSILON,
} SchGCStrategy;

extern const SchValue SCH_NULL, SCH_UNDEF, SCH_FALSE, SCH_TRUE;

void sch_init(uintptr_t *volatile base);
#define SCH_INIT() uintptr_t sch_stack_base = 0; sch_init((uintptr_t *) &sch_stack_base)
int sch_fin(void);

[[nodiscard, gnu::malloc, gnu::returns_nonnull]] char *sch_stringify(SchValue v);
void sch_display(SchValue v);
SchValue sch_parse(const char *path);
SchValue sch_parse_string(const char *in);
SchValue sch_load(const char *path);
SchValue sch_eval_string(const char *s);

const char *sch_error_message(void);

void sch_set_gc_init_size(double mib);
void sch_set_gc_stress(bool b);
void sch_set_gc_print_stat(bool b);
void sch_set_gc_strategy(SchGCStrategy s);

#endif
