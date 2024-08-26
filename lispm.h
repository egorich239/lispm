#pragma once

#include "rt.h"
#include "sym.h"

#define VERBOSE_FAILURES 1

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
#define LISPM_PAGE_PAGE_TABLE         LISPM_MAKE_PAGE(0) /* self-description */
#define LISPM_PAGE_PROGRAM            LISPM_MAKE_PAGE(1) /* lisp program code */
#define LISPM_PAGE_STACK              LISPM_MAKE_PAGE(2) /* execution stack */
#define LISPM_PAGE_INDEX              LISPM_MAKE_PAGE(3) /* hash table index */
#define LISPM_PAGE_STRINGS            LISPM_MAKE_PAGE(4) /* hash table strings */
#define LISPM_PAGE_TABLE_PRELUDE_SIZE 5

/*
   Arguments are provided as a SYM_NIL-terminated CONS-sequence.

   If evcap is non-zero, the builtin is treated as special form,
   i.e. it receives its args unevaluated.
   This callback is used to evaluate the list of args captured
   by this special form.
 */
struct Builtin {
  const char *name;
  Sym (*eval)(Sym args);
  Sym (*evcap)(Sym args, Sym caps);
};
#define BUILTINS_TABLE_SIZE 128u

/* The initial part of the strings table contains the error message string. */
#define ERROR_MESSAGE_SIZE 256u
_Static_assert(ERROR_MESSAGE_SIZE > 0, "error message size must be non-zero");

/* Strings index hashing is rather naive,
   and attempts to look at the next several slots.
   This value limits how many slots are looked up before we give up. */
#define STRINGS_INDEX_LOOKUP_LIMIT 32u

/* API */
Sym lispm_exec(struct PageDesc *page_table, unsigned offs, const struct Builtin *rt);

/* Integration */
/* Unlike ASSERT, these errors are caused by a bug in the user code. */
#if !VERBOSE_FAILURES
#define EVAL_CHECK(cond, err)                                                                                          \
  do {                                                                                                                 \
    if (!(cond)) lispm_report_error(err);                                                                              \
  } while (0)
#else
#define EVAL_CHECK(cond, err)                                                                                          \
  do {                                                                                                                 \
    if (!(cond)) {                                                                                                     \
      lispm_error_message_set("Failed assertion: " #cond);                                                             \
      lispm_report_error(err);                                                                                         \
    }                                                                                                                  \
  } while (0)
#endif
__attribute__((noreturn)) void lispm_report_error(Sym err);
void lispm_error_message_set(const char *msg);

/* pc must be from the PROGRAM page */
Sym lispm_parse(const char *pc);

struct PageDesc *lispm_page_desc(Sym pg);
void *lispm_page_loc(Sym pg, unsigned offs, unsigned elt_size);
unsigned lispm_page_size(Sym pg, int elt_size_log2);

Sym lispm_literal_name_span(Sym s);

Sym lispm_st_obj_alloc(unsigned k, Sym *vals);
static inline Sym lispm_cons_alloc(Sym car, Sym cdr) {
  Sym cons[2] = {car, cdr};
  return lispm_st_obj_alloc(LISPM_ST_OBJ_CONS, cons);
}

void lispm_st_obj_unpack2(Sym s, Sym *a, Sym *b);
void lispm_st_obj_unpack3(Sym s, Sym *a, Sym *b, Sym *c);

Sym lispm_alloc_cons(Sym car, Sym cdr);
void lispm_cons_unpack_user(Sym a, Sym *car, Sym *cdr);

Sym lispm_alloc_span(Sym page, Sym offs, Sym len);

Sym lispm_evcap_quote(Sym a, Sym c);
Sym lispm_evquote(Sym a);
void lispm_args_unpack2(Sym a, Sym *f, Sym *s);
