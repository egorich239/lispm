#pragma once

#include <liblispm/types.h>

enum LispmBuiltinFlags {
  /**
   * Forbids using the symbol as a value.
   * Mainly used for syntactic forms.
   */
  LISPM_BUILTIN_LITERAL_NOT_RVALUE = 1u,

  /**
   * Allows shadowing the value in a nested scope.
   * Mainly unset for builtin symbols, s.t. they cannot be redefined.
   */
  LISPM_BUILTIN_LITERAL_LVALUE = 2u,

  /**
   * Links the value of the literal to itself.
   * Mainly useful for `:keywords` and special symbols such as `#t`.
   */
  LISPM_BUILTIN_LITERAL_SELFREF = 4u,

  /**
   * Marks a syntax extension.
   */
  LISPM_BUILTIN_SYNTAX = 8u,
};

/**
 * We pack builtins into special sections, and rely on linker script to organize
 * them into a single contigous array, and as sections are 16-bytes aligned, we
 * align the struct similarly to avoid mishaps at concatenation of various
 * sections.
 */
struct __attribute__((aligned(16))) LispmBuiltin {
  const char *name;
  LispmObj (*eval)(LispmObj args);
  LispmObj (*aux)(LispmObj args);
  enum LispmBuiltinFlags flags;
};
/* Convenience macro to implement extensions */
#define LISPM_BUILTINS_EXT(name)                                                                                       \
  const struct LispmBuiltin name[] __attribute__((section(".lispm.rodata.builtins.ext"), aligned(16), used))
