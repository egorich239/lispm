#pragma once

#include "lispm.h"

/* Extension symbols: spans */
enum { LISPM_ST_OBJ_SPAN = LISPM_ST_OBJ_EXT };
static inline Sym lispm_make_span(unsigned st_offs) { return lispm_make_st_obj(LISPM_ST_OBJ_SPAN, st_offs); }
static inline int lispm_sym_is_span(Sym s) { return lispm_st_obj_kind(s) == LISPM_ST_OBJ_SPAN; }

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
