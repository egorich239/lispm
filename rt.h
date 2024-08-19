#pragma once

/* runtime, for now standard C one */
#include <setjmp.h>

extern jmp_buf main_try;

#define TRY(...)                                                               \
  do {                                                                         \
    if (!setjmp(main_try)) {                                                      \
      __VA_ARGS__;                                                             \
    }                                                                          \
  } while (0)

#define THROW(res) longjmp(main_try, 1)

