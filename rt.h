#pragma once

/* runtime, for now standard C one */
#include <setjmp.h>
#include <stdlib.h>

extern jmp_buf main_try;

#define ASSERT(cond)                                                                                                   \
  do {                                                                                                                 \
    if (!(cond)) __builtin_trap();                                                                                     \
  } while (0)

// #define ASSERT(cond) ((void)(cond))

#define FIN(code) exit(code)

#define TRY(...)                                                                                                       \
  do {                                                                                                                 \
    if (!setjmp(main_try)) { __VA_ARGS__; }                                                                            \
  } while (0)

#define THROW(res) longjmp(main_try, 1)

/* allocates read-writeable page */
void *page_alloc(unsigned size);
void page_release(void *page, unsigned size);
