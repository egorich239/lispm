#include "support.h"
#include "lispm.h"
#include "rt.h"
#include "sym.h"

static inline struct Page lispm_alloc_page(unsigned size) {
  struct Page res = {.begin = page_alloc(size)};
  if (!res.begin) return res;
  res.end = res.begin + size;
  return res;
}

struct Page *lispm_alloc_pages(const struct Page *program) {
  struct Page page0 = lispm_alloc_page(PAGE_PAGE_TABLE_SIZE);
  if (!page0.begin) return 0;
  struct Page *table = page0.begin;
  table[0] = page0;
  table[page_pt_offs(PAGE_PROGRAM)] = *program;
  table[page_pt_offs(PAGE_STACK)] = lispm_alloc_page(PAGE_STACK_SIZE);
  table[page_pt_offs(PAGE_INDEX)] = lispm_alloc_page(PAGE_INDEX_SIZE);
  table[page_pt_offs(PAGE_STRINGS)] = lispm_alloc_page(PAGE_STRINGS_SIZE);

  int success = 1;
  for (int i = 0; i < PAGE_TABLE_PRELUDE_SIZE; ++i) {
    success &= !!table[i].begin;
  }
  if (success) return page0.begin;
  lispm_release_pages(page0.begin);
  return 0;
}

void lispm_release_pages(const struct Page *table) {
  for (int i = 0; i < PAGE_TABLE_PRELUDE_SIZE; ++i) {
    page_release(table[i].begin, table[i].end - table[i].begin);
  }
}
