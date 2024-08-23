#pragma once

#include "lispm.h"

/* default sizes of the page allocated by lispm_alloc_pages() */
#define PAGE_PAGE_TABLE_SIZE 4096u
#define PAGE_STACK_SIZE      1048576u
#define PAGE_INDEX_SIZE      1048576u
#define PAGE_STRINGS_SIZE    1048576u

struct PageDesc *lispm_alloc_pages(const struct PageDesc *program);
void lispm_release_pages(const struct PageDesc* page0);