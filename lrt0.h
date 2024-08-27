#pragma once

#include "lispm.h"

void *lispm_page_loc(Sym pg, unsigned offs, unsigned elt_size);
static inline unsigned lispm_page_access(Sym pg) {
  LISPM_ASSERT(lispm_sym_is_shortnum(pg));
  const unsigned page = lispm_shortnum_val(pg);
  void *b, *e;
  unsigned access;
  return page <= 1 ? 0 : (lispm_rt_page(page, &b, &e, &access), access);
}
