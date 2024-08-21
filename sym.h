#pragma once

#include "rt.h"

/* symbol: last two bits encode its kind
 * - 0    ...    00: SYM_NIL
 * -      <OFFS> 00: symbol from the table, at '<OFFS>' position of the key
 *                   pointer;
 * -       <NUM> 01: for atoms that are decimal representation of bits of <NUM>;
 * -   <CONS> 00 10: stack position of the pair car (CONS), cdr (CONS+1);
 * - <LAMBDA> 01 10: stack position of the triplet captures (LAMBDA), args (+1),
 *                   body (+2);
 *
 * The 11 trailing bits are reserved for special symbols;
 * all of them have upper bit zero, which is used by hash table to
 * mark the literals that cannot be overriden by a locally bound variable.
 *
 * - 0 <OFFS> 00 11: builtin function at the specific offset in the ftable;
 * - 0    ... 11 11: special forms (T, CONS, LET, ...)
 *              ...: (assuming 32-bit unsigned)
 *        7FFF FFDF: used during evaluation of capture lists of LET;
 *                   marks a literal first defined in the previous part of
 *                   the LET evaluations;
 *        7FFF FFEF: used during evaluation of capture lists of LAMBDA/LET;
 *                   marks a literal already in the capture list;
 *        7FFF FFFF: no value currently associated with the literal.
 */
typedef unsigned Sym;

#define UPPER_BITS(n) ~(~0u >> (n))

/* atoms */
static inline int is_atom(Sym s) { return (s & 2u) == 0; }

/* literals */
static inline Sym make_literal(unsigned ht_offs) { return ht_offs << 2; }
static inline int is_literal(Sym s) { return (s & 3u) == 0; }
static inline unsigned literal_ht_offs(Sym s) {
  ASSERT(is_literal(s));
  return s >> 2;
}

/* unsigneds */
static inline Sym make_unsigned(unsigned val) {
  ASSERT((val & UPPER_BITS(2)) == 0);
  return (val << 2) | 1u;
}
static inline int is_unsigned(Sym s) { return (s & 3u) == 1; }
static inline unsigned unsigned_val(Sym s) {
  ASSERT(is_unsigned(s));
  return s >> 2;
}
static inline Sym unsigned_add(Sym a, Sym b, int *oflow) {
  Sym res;
  *oflow = __builtin_uadd_overflow(a, b, &res);
  return res ^ 3u;
}
static inline Sym unsigned_mul(Sym a, Sym b, int *oflow) {
  Sym res;
  *oflow = __builtin_uadd_overflow(unsigned_val(a), unsigned_val(b), &res) |
           (res & UPPER_BITS(2));
  return make_unsigned(res);
}

/* stack objects*/
static inline int is_st_obj(Sym s) { return (s & 3u) == 2; }
static inline unsigned st_obj_st_offs(Sym s) {
  ASSERT(is_st_obj(s));
  return s >> 4;
}
static inline Sym st_obj_offset_by(Sym s, unsigned offs) {
  /* utility for gc */
  ASSERT(is_st_obj(s));
  return s + (offs << 4);
}

/* cons */
static inline Sym make_cons(unsigned st_offs) { return (st_offs << 4) | 2u; }
static inline int is_cons(Sym s) { return (s & 15u) == 2u; }
static inline unsigned cons_st_offs(Sym s) {
  ASSERT(is_cons(s));
  return st_obj_st_offs(s);
}

/* lambda */
static inline Sym make_lambda(unsigned st_offs) { return (st_offs << 4) | 6u; }
static inline int is_lambda(Sym s) { return (s & 15u) == 6u; }
static inline unsigned lambda_st_offs(Sym s) {
  ASSERT(is_lambda(s));
  return st_obj_st_offs(s);
}

/* specials, ctors are defined in macros, to be compile time consts */
#define SPECIAL_READONLY_BIT UPPER_BITS(1)
static inline int is_special(Sym s) { return (s & 3u) == 3u; }
static inline int special_is_readonly(Sym s) {
  ASSERT(is_special(s));
  return s & SPECIAL_READONLY_BIT;
}

/* builtin functions */
#define MAKE_BUILTIN_FN(ft_offs)                                               \
  ((((ft_offs) << 4) | 3u) | SPECIAL_READONLY_BIT)
static inline int is_builtin_fn(Sym s) { return (s & 15u) == 3u; }
static inline unsigned builtin_fn_ft_offs(Sym s) {
  ASSERT(is_builtin_fn(s));
  return (s & ~SPECIAL_READONLY_BIT) >> 4;
}

/* special forms */
#define MAKE_SPECIAL_FORM(val) ((((val) << 4) | 15u) & ~SPECIAL_READONLY_BIT)

#define SYM_NIL    0u
#define SYM_T      (MAKE_SPECIAL_FORM(0) | SPECIAL_READONLY_BIT)
#define SYM_QUOTE  (MAKE_SPECIAL_FORM(1) | SPECIAL_READONLY_BIT)
#define SYM_COND   (MAKE_SPECIAL_FORM(2) | SPECIAL_READONLY_BIT)
#define SYM_LAMBDA (MAKE_SPECIAL_FORM(3) | SPECIAL_READONLY_BIT)
#define SYM_LET    (MAKE_SPECIAL_FORM(4) | SPECIAL_READONLY_BIT)

#define SYM_BINDING  MAKE_SPECIAL_FORM(~0u - 2)
#define SYM_CAPTURED MAKE_SPECIAL_FORM(~0u - 1)
#define SYM_NO_ASSOC MAKE_SPECIAL_FORM(~0u - 0)
