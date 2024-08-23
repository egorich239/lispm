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
struct PageDesc {
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

/* API */
Sym lispm_exec(struct PageDesc *page_table, unsigned offs);

/* Integration */

/* Unlike ASSERT, these errors are caused by a bug in the user code. */
#define EVAL_CHECK(cond, err)                                                  \
  do {                                                                         \
    if (!(cond)) lispm_report_error(err);                                      \
  } while (0)

/* builtins: functions and special forms */
struct Builtin {
  const char *name;
  Sym (*fn)(Sym args);
  Sym (*evcap)(Sym args, Sym caps);
};

__attribute__((noreturn)) void lispm_report_error(Sym err);

struct PageDesc *lispm_page_desc(Sym pg);
void *lispm_page_loc(Sym pg, unsigned offs, unsigned elt_size);
unsigned lispm_page_size(Sym pg, int elt_size_log2);

Sym lispm_alloc_cons(Sym car, Sym cdr);
void lispm_cons_unpack(Sym a, Sym *car, Sym *cdr);
void lispm_cons_unpack_user(Sym a, Sym *car, Sym *cdr);

Sym lispm_alloc_pointer(Sym page, Sym offs, Sym len);
void lispm_pointer_unpack(Sym ptr, Sym *page, Sym *offs, Sym *len);

Sym lispm_evquote(Sym a);
void lispm_args_unpack2(Sym a, Sym *f, Sym *s);
