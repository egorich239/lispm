#include "support.h"
#include "lispm.h"
#include "rt.h"
#include "sym.h"

static inline struct PageDesc lispm_alloc_page(unsigned size) {
  struct PageDesc res = {.begin = page_alloc(size)};
  if (!res.begin) return res;
  res.end = res.begin + size;
  return res;
}

struct PageDesc *lispm_alloc_pages(const struct PageDesc *program) {
  struct PageDesc page0 = lispm_alloc_page(PAGE_PAGE_TABLE_SIZE);
  if (!page0.begin) return 0;
  struct PageDesc *table = page0.begin;
  table[0] = page0;
  table[lispm_page_pt_offs(LISPM_PAGE_PROGRAM)] = *program;
  table[lispm_page_pt_offs(LISPM_PAGE_STACK)] = lispm_alloc_page(PAGE_STACK_SIZE);
  table[lispm_page_pt_offs(LISPM_PAGE_INDEX)] = lispm_alloc_page(PAGE_INDEX_SIZE);
  table[lispm_page_pt_offs(LISPM_PAGE_STRINGS)] = lispm_alloc_page(PAGE_STRINGS_SIZE);

  int success = 1;
  for (int i = 0; i < LISPM_PAGE_TABLE_PRELUDE_SIZE; ++i)
    success &= !!table[i].begin;
  if (success) return page0.begin;
  lispm_release_pages(page0.begin);
  return 0;
}

void lispm_release_pages(const struct PageDesc *table) {
  for (int i = 0; i < LISPM_PAGE_TABLE_PRELUDE_SIZE; ++i) {
    page_release(table[i].begin, table[i].end - table[i].begin);
  }
}
