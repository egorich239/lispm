#pragma once

#include "lispm.h"

/* Extension symbols: spans 
 * -     <SPAN> 11 10: triplet of short unsigned's page, offs, length; see lrt0.h for more.
 */
enum { LISPM_ST_OBJ_SPAN = LISPM_ST_OBJ_EXT_3 };
static inline int lispm_sym_is_span(Sym s) { return lispm_st_obj_kind(s) == LISPM_ST_OBJ_SPAN; }

struct Span {
  Sym page; /* shortnum */
  Sym offs; /* shortnum */
  Sym len;  /* shortnum */
};

static inline struct Span lispm_make_span(unsigned page, unsigned offs, unsigned len) {
  LISPM_ASSERT(lispm_shortnum_can_represent(page) && lispm_shortnum_can_represent(offs) &&
               lispm_shortnum_can_represent(len));
  return (struct Span){lispm_make_shortnum(page), lispm_make_shortnum(offs), lispm_make_shortnum(len)};
}
static inline Sym lispm_span_alloc(struct Span span) {
  /* Casting would be so much nicer, but technically UB. Let the optimizer care. */
  Sym arr[] = {span.page, span.offs, span.len};
  return lispm_st_obj_alloc(LISPM_ST_OBJ_SPAN, arr);
}
static inline struct Span lispm_span_unpack(Sym span) {
  LISPM_ASSERT(lispm_sym_is_span(span));
  Sym *arr = lispm_st_obj_unpack(span);
  return (struct Span){arr[0], arr[1], arr[2]};
}

/* Page access from the perspective of a LISP program. */
enum PageAccessFlags {
  /* The page is readable */
  PAGE_ACCESS_R = 1,
  /* The page is writable */
  PAGE_ACCESS_W = 2,
  /* The page contains native platform code */
  PAGE_ACCESS_X = 4,
};

/* Callback to get the properties of a page.
   IDs 0 and 1 are reserved for program and strings pages, this function is never called with these ids. */
extern void lispm_rt_page(unsigned id, void **begin, void **end, unsigned *page_access);
