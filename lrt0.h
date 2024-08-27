#pragma once

#include "lispm.h"

/* Page access from the perspective of a LISP program. */
enum PageAccessFlags {
  /* The page is writable */
  PAGE_ACCESS_W = 2,
  /* The page contains native platform code */
  PAGE_ACCESS_X = 4,
};

/* Callback to get the properties of a page.
   IDs 0 and 1 are reserved for program and strings pages, this function is never called with these ids. */
extern void lispm_rt_page(unsigned id, void **begin, void **end, unsigned *page_access);

void *lispm_page_loc(Sym pg, unsigned offs, unsigned elt_size);
static inline unsigned lispm_page_access(Sym pg) {
  LISPM_ASSERT(lispm_sym_is_shortnum(pg));
  const unsigned page = lispm_shortnum_val(pg);
  void *b, *e;
  unsigned access;
  return page <= 1 ? 0 : (lispm_rt_page(page, &b, &e, &access), access);
}
