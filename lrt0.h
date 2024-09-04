#pragma once

#include "lispm-obj.h"
#include "lispm.h"

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
