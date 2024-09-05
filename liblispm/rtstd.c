#include <liblispm/rt.h>

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

void *lispm_rt_stack_mark(void) { return __builtin_frame_address(0); }
int lispm_rt_stack_depth(void *mark) {
  ptrdiff_t res = __builtin_frame_address(0) - mark;
  return res < 0 ? -res : res; /* I assume no frame takes half of addressable space */
}
