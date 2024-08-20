#pragma once

#include "rt.h"

#define STATUS_OK 0
#define STATUS_OOM 1
#define STATUS_LEX 2
#define STATUS_PARSE 3
#define STATUS_EVAL 4

/* Page must be aligned to CPU page size */
struct Page {
  char *begin;
  char *end;
};

#define PAGE_TABLE_SIZE 1024u

#define STRINGS_SIZE 0x100000u
_Static_assert(!(STRINGS_SIZE & 4095),
               "main table size must be proportional to 4K pages");

#define STRINGS_INDEX_SIZE 1048576u
_Static_assert(!(STRINGS_INDEX_SIZE & (STRINGS_INDEX_SIZE - 1)),
               "hash table size must be a power of two");

/* Strings index hashing is rather naive,
   and attempts to look at the next several slots.
   This value limits how many slots are looked up before we give up. */
#define STRINGS_INDEX_LOOKUP_LIMIT 32u

/* stack */
#define STACK_SIZE 0x10000u

#define PARSE_DEPTH_LIMIT 1023u
_Static_assert(!(PARSE_DEPTH_LIMIT & (PARSE_DEPTH_LIMIT + 1)),
               "parse depth limit must be a factor of two minus one");
__attribute__((noreturn)) void lispm_start(struct Page *program);