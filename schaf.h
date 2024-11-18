#ifndef SCHAF_H
#define SCHAF_H

#include <stdbool.h>
#include <stdint.h>

#include "utils.h"

typedef enum {
// immediate
    SCH_TYPE_BOOL,
    SCH_TYPE_INT,
    SCH_TYPE_SYMBOL,
    SCH_TYPE_UNDEF,
// boxed (tagged)
    SCH_TYPE_PAIR,
    SCH_TYPE_STR,
    SCH_TYPE_PROC,
} SchType;

typedef uintptr_t SchValue;
typedef uintptr_t SchSymbol;

extern const SchValue SCH_Qnil, SCH_Qundef, SCH_Qfalse, SCH_Qtrue;

bool sch_value_is_int(SchValue v);
bool sch_value_is_symbol(SchValue v);
bool sch_value_is_string(SchValue v);
bool sch_value_is_pair(SchValue v);
SchType sch_value_type_of(SchValue v);

int64_t sch_value_to_int(SchValue v);
SchSymbol sch_value_to_symbol(SchValue v);
const char *sch_value_to_string(SchValue v);

SchValue sch_value_of_int(int64_t i);
SchValue sch_value_of_symbol(const char *s);
SchValue sch_value_of_string(const char *s);

SchValue sch_cons(SchValue car, SchValue cdr);
int64_t sch_length(SchValue list);

SchValue sch_car(SchValue v);
SchValue sch_cdr(SchValue v);

ATTR_MALLOC char *sch_stringify(SchValue v);
void sch_display(SchValue v);
SchValue sch_parse(const char *path);
SchValue sch_parse_string(const char *in);
SchValue sch_load(const char *path);
SchValue sch_eval_string(const char *s);

const char *sch_error_message(void);

#endif
