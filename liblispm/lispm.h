#pragma once

#include <liblispm/builtins.h>
#include <liblispm/obj.h>
#include <liblispm/rt.h>
#include <liblispm/trace.h>
#include <liblispm/types.h>

enum {
  /* The bottom of the stack is used to communicate information about errors. */
  LISPM_STACK_BOTTOM_OFFSET = 8u,

  /* The limit on the length of a builtin name. */
  LISPM_BUILTIN_NAME_LIMIT = 15u,
};

/* API */
static inline int lispm_is_power_of_two(unsigned i) { return i && !(i & (i - 1)); }
static inline int lispm_is_valid_config(void) {
  const struct Lispm *m = &lispm;
  return m->stack + LISPM_STACK_BOTTOM_OFFSET < m->stack_end /**/
         && m->strings + 8 <= m->strings_end                 /**/
         && m->htable + 1024 <= m->htable_end                /**/
         && lispm_is_power_of_two(m->htable_end - m->htable) /**/
         && m->stack_depth_limit >= 1024u                    /**/
         /* we also want all offsets to fit into short unsigned */
         && lispm_shortnum_can_represent(m->stack_end - m->stack)     /**/
         && lispm_shortnum_can_represent(m->strings_end - m->strings) /**/
         && lispm_shortnum_can_represent(m->htable_end - m->htable);
}

/**
 * Initializes the state of lispm object.
 *
 * Returns 1 on success, 0 otherwise.
 */
int lispm_init(void);

/**
 * Parses and executes the program, located at `lispm.pc`.
 * If the lexing, parsing or evaluation causes an error, then error symbol is returned.
 *
 * May not be called recursively. If you need to trigger evaluation from a language extension,
 * use `lispm_eval0()`.
 */
LispmObj lispm_eval(void);

/**
 * Parses and executes the program, located at `lispm.pc`.
 * If the lexing or parsing causes an error, then error symbol is returned.
 *
 * May not be called recursively. If you need to trigger evaluation from a language extension,
 * use `lispm_parse_quote0()`.
 */
LispmObj lispm_parse_quote(void);

/* Internal API. Zero in the function name denotes that it can cause runtime error, and must be executed within
 * `lispm_rt_try()` context. */

/**
 * Terminates the VM with `#err!`, puts `ctx` at `lispm.stack[1]`.
 */
__attribute__((noreturn)) void lispm_panic0(LispmObj ctx);

/**
 * Parses an S-expression, starting at `pc`, and terminating no later than `pc_end`.
 *
 * Terminates the VM wirh `#err!` if `pc_end` has been reached during parsing, or any other parse error occured.
 */
LispmObj lispm_parse_quote0(void);

/**
 * Parses and executes S-expression in the current context of the VM.
 *
 * Terminates the VM wirh `#err!` if `pc_end` has been reached during parsing, any other parse or runtime error occured.
 */
LispmObj lispm_eval0(void);

/**
 * Use this function to return `obj` as a result of an extension evaluation.
 */
LispmObj lispm_return0(LispmObj obj);

/**
 * Outputs up to `limit` first elements of (potentially empty) list `li` into `out` array.
 * Returns the number of elements in the list, if the list is shorter than limit, otherwise returns `~0u`.
 */
unsigned lispm_list_scan(LispmObj *out, LispmObj li, unsigned limit);
