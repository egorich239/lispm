#pragma once

#include "rt.h"
#include "symtable.h"

/* Page must be aligned to CPU page size */
struct Page {
  char *begin;
  char *end;
};

#define PAGE_TABLE_SIZE 1024u

#define STRINGS_SIZE 0x100000u
_Static_assert(!(STRINGS_SIZE & 4095),
               "main table size must be proportional to 4K pages");

#define HTABLE_SIZE 1048576u
_Static_assert(!(HTABLE_SIZE & (HTABLE_SIZE - 1)),
               "hash table size must be a power of two");

__attribute__((noreturn)) void lispm_start(struct Page* program);


/* stack */
#define STACK_SIZE 0x10000u
