#pragma once

#include "lispm.h"

void lispm_dump(const struct PageDesc *table, Sym sym);
const char *lispm_error_message_get(const struct PageDesc *table);
