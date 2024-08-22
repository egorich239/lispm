#pragma once

#include "rt.h"
#include "sym.h"

/* NOTE! These are access flags from the perspective of the Lisp program,
         the underlying pages may be less restrictive, and indeed will be
         for all interpreter's pages, potentially, except for the
         PAGE_PROGRAM which is never accessed for writing by us. */
#define PAGE_FLAG_RO 0u
#define PAGE_FLAG_RW 1u
#define PAGE_FLAG_RX 2u /* not executable! */

/* Page must be aligned to CPU page size */
struct Page {
  void *begin;
  void *end;
  unsigned flags;
};

/* pages in page table */
#define PAGE_PAGE_TABLE         MAKE_PAGE(0) /* self-description */
#define PAGE_PROGRAM            MAKE_PAGE(1) /* lisp program code */
#define PAGE_STACK              MAKE_PAGE(2) /* stack, at least 1024 unsigned's */
#define PAGE_INDEX              MAKE_PAGE(3) /* hash table index */
#define PAGE_STRINGS            MAKE_PAGE(4) /* hash table strings */
#define PAGE_TABLE_PRELUDE_SIZE 5

/* Strings index hashing is rather naive,
   and attempts to look at the next several slots.
   This value limits how many slots are looked up before we give up. */
#define STRINGS_INDEX_LOOKUP_LIMIT 32u

Sym lispm_exec(struct Page *page_table, unsigned offs);
