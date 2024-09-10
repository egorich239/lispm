#pragma once

#include <liblispm/types.h>

/* Abort is an external symbol provided by runtime */
extern __attribute__((noreturn)) void lispm_rt_abort(void);

/* Invokes `fn` and returns its result, unless `fn` calls `lispm_rt_throw(res)`.
   In the latter case, the evaluation of `fn` is stopped, the stack is unwinded,
   and `lispm_rt_try()` returns `res` passed to `lispm_rt_throw()`.

   The function is allowed to terminate if it detects nested invocation.
 */
extern unsigned lispm_rt_try(unsigned (*fn)(void));
/* Causes immediate return from the `lispm_rt_try()` call with the passed result.
   If called outside of `lispm_rt_try()` call, the result is undefined. */
extern __attribute__((noreturn)) void lispm_rt_throw(unsigned result);

extern void *lispm_rt_stack_mark(void);
extern int lispm_rt_stack_depth(void *mark);

/* Machine must be initialized before calling lispm_exec(). */
extern struct Lispm lispm;
