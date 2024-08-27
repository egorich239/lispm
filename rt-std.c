#include "lispm.h"
#include <setjmp.h>
#include <stddef.h>
#include <stdlib.h>

static jmp_buf rt_try;

__attribute__((noreturn)) void lispm_rt_abort() { abort(); }
void lispm_rt_page(unsigned _id, void **_b, void **_e, unsigned *_pa) { lispm_rt_abort(); }
void lispm_rt_try(void (*fn)(void)) {
  if (!setjmp(rt_try)) (*fn)();
}
__attribute__((noreturn)) void lispm_rt_throw(void) { longjmp(rt_try, 1); }
