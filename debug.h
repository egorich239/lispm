#pragma once

#include "lispm.h"

void lispm_dump(Sym sym);
void lispm_print_short(Sym sym);
const char *lispm_error_message_get(void);
