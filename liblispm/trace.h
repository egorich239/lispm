#pragma once

#include <liblispm/config.h>
#include <liblispm/rt.h>
#include <liblispm/types.h>

#if LISPM_CONFIG_VERBOSE
extern struct LispmTraceCallbacks lispm_trace;
#define LISPM_TRACE(event, ...)                                                                                        \
  do {                                                                                                                 \
    if (lispm_trace.event) lispm_trace.event(__VA_ARGS__);                                                             \
  } while (0)
#else
#define LISPM_TRACE(...) ((void)0)
#endif

#if LISPM_CONFIG_ASSERT
#define LISPM_ASSERT(cond)                                                                                             \
  do {                                                                                                                 \
    if (!(cond)) {                                                                                                     \
      LISPM_TRACE(assertion, __FILE__, __LINE__, #cond);                                                               \
      lispm_rt_abort();                                                                                                \
    }                                                                                                                  \
  } while (0)
#else
#define LISPM_ASSERT(cond) ((void)(0))
// #define LISPM_ASSERT(cond) if (!(cond)) __builtin_unreachable();
#endif

/* Unlike LISPM_ASSERT, these errors are caused by a bug in the user code. */
#define LISPM_EVAL_CHECK(cond, ctx, event, ...)                                                                        \
  do {                                                                                                                 \
    if (!(cond)) {                                                                                                     \
      LISPM_TRACE(event, __FILE__, __LINE__, ##__VA_ARGS__);                                                           \
      lispm_panic0(ctx);                                                                                               \
    }                                                                                                                  \
  } while (0)
