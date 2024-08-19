#include <setjmp.h>
#include <stddef.h>
#include <sys/mman.h>

#include "rt.h"

jmp_buf main_try;

void *page_alloc(unsigned size) {
  ASSERT(!(size & (size - 1)));
  return mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS,
              -1, 0);
}