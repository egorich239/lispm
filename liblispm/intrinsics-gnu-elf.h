#pragma once

#include <stddef.h>

#include <liblispm/trace.h>

/* builtins */
#define LISPM_INTRINSIC_BUILTINS_DETAIL()                                                                              \
  extern struct LispmBuiltin lispm_builtins_start[];                                                                   \
  extern struct LispmBuiltin lispm_builtins_end[]

#define lispm_intrinsic_builtin_size() ((unsigned)(lispm_builtins_end - lispm_builtins_start))
#define lispm_intrinsic_builtin_offset(bi_)                                                                            \
  ({                                                                                                                   \
    LISPM_ASSERT(lispm_builtins_start <= (bi_) && (bi_) < lispm_builtins_end);                                         \
    ((bi_) - lispm_builtins_start);                                                                                    \
  })
#define lispm_intrinsic_builtin_at(offs_)                                                                              \
  ({                                                                                                                   \
    LISPM_ASSERT(0 <= ((unsigned)(offs_)) && ((unsigned)(offs_)) < lispm_intrinsic_builtin_size());                    \
    (lispm_builtins_start + ((unsigned)(offs_)));                                                                      \
  })
#define LISPM_INTRINSIC_BUILTINS_CORE(name_)                                                                           \
  const struct LispmBuiltin name_[] __attribute__((section(".lispm.rodata.builtins.core"), aligned(16), used))
#define LISPM_INTRINSIC_BUILTINS_EXT(name_)                                                                            \
  const struct LispmBuiltin name_[] __attribute__((section(".lispm.rodata.builtins.ext"), aligned(16), used))
#define LISPM_INTRINSIC_BUILTINS_LINK(...) _Static_assert(1, "Do not forget to use the linker script")

/* various builtins */
#define lispm_intrinsic_strcmp(a_, b_)    __builtin_strcmp((a_), (b_))
#define lispm_intrinsic_uadd(a_, b_, c_)  __builtin_uadd_overflow((a_), (b_), (c_))
#define lispm_intrinsic_umul(a_, b_, c_)  __builtin_umul_overflow((a_), (b_), (c_))
#define lispm_intrinsic_leading_zeros(a_) __builtin_clz(a_)
