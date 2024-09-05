#pragma once

#include <liblispm/types.h>

/*
 * Arguments are provided as a SYM_NIL-terminated CONS-sequence.
 *
 * If evcap is non-zero, the builtin is treated as special form, i.e. it
 * receives its args unevaluated. This callback is used to evaluate the list of
 * args captured by this special form.
 *
 * We pack builtins into special sections, and rely on linker script to organize
 * them into a single contigous array, and as sections are 16-bytes aligned, we
 * align the struct similarly to avoid mishaps at concatenation of various
 * sections.
 */
struct __attribute__((aligned(16))) LispmBuiltin {
  const char *name;
  LispmObj (*eval)(LispmObj args);
  LispmObj (*sema)(LispmObj args);
};
/* Convenience macro to implement extensions */
#define LISPM_BUILTINS_EXT(name)                                                                                       \
  const struct LispmBuiltin name[] __attribute__((section(".lispm.rodata.builtins.ext"), aligned(16), used))
