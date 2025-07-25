#ifndef SCHAF_H
#define SCHAF_H

#include <stdbool.h>
#include <stdint.h>

typedef uintptr_t Value;

extern const Value Qnil, Qundef, Qfalse, Qtrue;

void sch_init(uintptr_t *base);
#define SCH_INIT() uintptr_t sch_stack_base = 0; sch_init(&sch_stack_base)
int sch_fin(void);

[[nodiscard, gnu::malloc, gnu::returns_nonnull]] char *stringify(Value v);
void display(Value v);
Value parse(const char *path);
Value parse_string(const char *in);
Value load(const char *path);
Value eval_string(const char *s);

const char *error_message(void);

void sch_set_gc_init_size(double mib);
void sch_set_gc_stress(bool b);

#endif
