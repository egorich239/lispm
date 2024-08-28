#pragma once

#include "lispm.h"

void lispm_trace(void);
void lispm_trace_full(void);

void lispm_dump(Sym sym);
void lispm_print_short(Sym sym);
void lispm_print_stack_trace(void);
const char *lispm_error_message_get(void);
