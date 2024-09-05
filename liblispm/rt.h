#pragma once

#include <liblispm/types.h>

/* Abort is an external symbol provided by runtime */
extern __attribute__((noreturn)) void lispm_rt_abort(void);
/* Invokes `fn` such that a call to `lispm_rt_throw()` causes immediate stack unwinding
   and return from the `lispm_rt_try` function. The library invokes this function exactly
   once. */
extern void lispm_rt_try(void (*fn)(void));
/* Causes immediate return from the `lispm_rt_try()` call.
   Guaranteed to be called only from within `lispm_rt_try` function. */
extern __attribute__((noreturn)) void lispm_rt_throw(void);

/* Machine must be initialized before calling lispm_exec(). */
extern struct Lispm lispm;
