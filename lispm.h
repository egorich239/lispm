#pragma once

#include "symtable.h"

#define TABLE_SIZE 0x100000u
_Static_assert((TABLE_SIZE & 3) == 0);

#define LISPM_ASSERT(cond, err)                                                \
  if (!(cond))                                                                 \
    lispm_halt(err);

/* we expect TABLE initialized with TABLE_PREFIX from symtable.h */
extern char TABLE[];

/* symbol: last two bits encode its kind
 * - <OFFS> 00: symbol from the table, at '<OFFS> 00' position;
 * -  <NUM> 10: for tokens that represent decimal representation of bits of
 * <NUM>;
 * - <CONS> 01: (CONS car cdr), stored at stack CONS (car) and CONS+1 (cdr)
 * position;
 * - <BITS> 11: special marker.
 */
typedef unsigned Sym;

/* stack */
#define STACK_SIZE 0x10000u
extern unsigned stack_pointer;
extern Sym STACK[];

/* error */
extern __attribute__((noreturn)) void lispm_halt(Sym err);
/* success: table is the output */
extern __attribute__((noreturn)) void lispm_done(void);
/* capi callback, if present */
extern Sym (*lispm_capi)(Sym (*eval)(Sym, Sym), Sym, Sym);

/* is atom? */
static inline Sym Atom(Sym x) { return !(x & 1u) ? SYM_T : SYM_NIL; }
/* is unsigned? */
static inline Sym Unsigned(Sym x) { return (x & 3) == 2 ? SYM_T : SYM_NIL; }
/* is list? */
static inline Sym List(Sym x) { return (x & 3u) == 1 ? SYM_T : SYM_NIL; }
/* is special? */
static inline Sym Special(Sym x) { return (x & 3u) == 3 ? SYM_T : SYM_NIL; }

/* ints */
static inline Sym MakeUnsigned(unsigned val) {
  LISPM_ASSERT(((val << 2) >> 2) == val, ERR_EVAL);
  return (val << 2) | 2;
}
static inline unsigned GetUnsigned(Sym val) {
  LISPM_ASSERT(Unsigned(val), ERR_EVAL);
  return val >> 2;
}

/* lisp style Eq */
static inline Sym Eq(Sym x, Sym y) { return Atom(x) ? x == y : SYM_NIL; }

/* list routines */
/* list constructor */
static inline Sym Cons(Sym car, Sym cdr) {
  LISPM_ASSERT(stack_pointer >= 2, ERR_OOM);
  STACK[--stack_pointer] = cdr;
  STACK[--stack_pointer] = car;
  return (stack_pointer << 2) | 1;
}

static inline Sym Car(Sym a) { return List(a) ? STACK[(a >> 2) + 0] : SYM_NIL; }
static inline Sym Cdr(Sym a) { return List(a) ? STACK[(a >> 2) + 1] : SYM_NIL; }
static inline Sym Caar(Sym a) { return Car(Car(a)); }
static inline Sym Cadr(Sym a) { return Car(Cdr(a)); }
static inline Sym Cdar(Sym a) { return Cdr(Car(a)); }
static inline Sym Cddr(Sym a) { return Cdr(Cdr(a)); }
