#pragma once

#include <liblispm/lispm.h>

void lispm_trace_stack(void);
void lispm_trace_full(void);

void lispm_dump(LispmObj sym);
void lispm_print_short(LispmObj sym);
void lispm_print_stack_trace(void);
const char *lispm_error_message_get(void);

void lispm_reset_runtime_stats(void);
void lispm_print_runtime_stats(void);
