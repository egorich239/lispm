#pragma once

#include "rt.h"

/* Symbol: last two bits encode its kind
 * - 0    ...    00: LISPM_SYM_NIL
 * - 0    <OFFS> 00: builtin symbol in htable, key pointer at '<OFFS>';
 * - 1    <OFFS> 00: user-defined symbol in htable, key pointer at '<OFFS>';
 * -       <NUM> 01: for atoms that are decimal representation of bits of <NUM>;
 * -   <CONS> 00 10: stack position of the pair car (CONS), cdr (CONS+1);
 * - <LAMBDA> 01 10: stack position of the triplet captures (LAMBDA), args (+1),
 *                   body (+2);
 * -    <PTR> 11 10: stack position of the triplet page (PTR), offs (PTR+1),
 *                   length of memory range;
 *                   page is the page symbol (see below),
 *                   offs is byte offset at this page, encoded as numerical;
 *                   memory range encodes the range as numerical.
 *
 * The 11 trailing bits are reserved for special symbols.
 *
 * - F   <OFFS> 00 11: builtin function at the specific offset in the ftable;
 *                     when F bit is 1, it denotes a special form;
 *                     special forms receive their params unevaluated.
 * -     <OFFS> 01 11: page, offs specifies offset in page table;
 *
 * -        ... 11 11: special values (assuming 32-bit unsigned)
 *          0000 000F: T
 *          FFFF FFDF: used during evaluation of capture lists of LET;
 *                   marks a literal first defined in the previous part of
 *                   the LET evaluations;
 *          FFFF FFEF: used during evaluation of capture lists of LAMBDA/LET;
 *                   marks a literal already in the capture list;
 *          FFFF FFFF: no value currently associated with the literal.
 */
typedef unsigned Sym;

#define UPPER_BITS(n) ~(~0u >> (n))

/* atoms */
static inline int lispm_sym_is_nil(Sym s) { return !s; }
static inline int lispm_sym_is_atom(Sym s) { return (s & 2u) == 0; }

/* literals */
static inline Sym lispm_make_literal(unsigned ht_offs) { return ht_offs << 2; }
static inline int lispm_sym_is_literal(Sym s) { return (s & 3u) == 0; }
static inline unsigned lispm_literal_ht_offs(Sym s) {
  ASSERT(lispm_sym_is_literal(s));
  return s >> 2;
}

/* unsigneds */
static inline Sym lispm_make_unsigned(unsigned val) {
  ASSERT((val & UPPER_BITS(2)) == 0);
  return (val << 2) | 1u;
}
static inline int lispm_sym_is_unsigned(Sym s) { return (s & 3u) == 1; }
static inline unsigned lispm_unsigned_val(Sym s) {
  ASSERT(lispm_sym_is_unsigned(s));
  return s >> 2;
}
static inline Sym lispm_unsigned_add(Sym a, Sym b, int *oflow) {
  ASSERT(lispm_sym_is_unsigned(a) && lispm_sym_is_unsigned(b));
  Sym res;
  *oflow = __builtin_uadd_overflow(a, b, &res);
  return res ^ 3u;
}
static inline Sym lispm_unsigned_sub(Sym a, Sym b, int *oflow) {
  ASSERT(lispm_sym_is_unsigned(a) && lispm_sym_is_unsigned(b));
  Sym res;
  *oflow = __builtin_usub_overflow(a, b, &res);
  return res | 1u;
}
static inline Sym lispm_unsigned_mul(Sym a, Sym b, int *oflow) {
  ASSERT(lispm_sym_is_unsigned(a) && lispm_sym_is_unsigned(b));
  unsigned res;
  *oflow = __builtin_umul_overflow(lispm_unsigned_val(a), lispm_unsigned_val(b), &res) | (res & UPPER_BITS(2));
  return lispm_make_unsigned(res & ~UPPER_BITS(2));
}
static inline Sym lispm_unsigned_band(Sym a, Sym b) {
  ASSERT(lispm_sym_is_unsigned(a) && lispm_sym_is_unsigned(b));
  return a & b;
}
static inline Sym lispm_unsigned_bor(Sym a, Sym b) {
  ASSERT(lispm_sym_is_unsigned(a) && lispm_sym_is_unsigned(b));
  return a | b;
}
static inline Sym lispm_unsigned_bxor(Sym a, Sym b) {
  ASSERT(lispm_sym_is_unsigned(a) && lispm_sym_is_unsigned(b));
  return a ^ b ^ 1;
}
static inline Sym lispm_unsigned_bnot(Sym a) {
  ASSERT(lispm_sym_is_unsigned(a));
  return ~a ^ 3u;
}

/* stack objects*/
#define LISPM_ST_OBJ_CONS   2u
#define LISPM_ST_OBJ_LAMBDA 6u
#define LISPM_ST_OBJ_SPAN   14u
static inline Sym lispm_make_st_obj(unsigned k, unsigned st_offs) {
  ASSERT(k == LISPM_ST_OBJ_CONS || k == LISPM_ST_OBJ_LAMBDA || k == LISPM_ST_OBJ_SPAN);
  return (st_offs << 4) | k;
}
static inline int lispm_sym_is_st_obj(Sym s) { return (s & 3u) == 2; }
static inline unsigned lispm_st_obj_kind(Sym s) { return s & 15u; }
static inline unsigned lispm_st_obj_st_size(Sym s) {
  /* size of the object on the stack, in words */
  return (s & 4) ? 3 : 2; /* ((s&4) >> 2) + 2 ?? */
}
static inline unsigned lispm_st_obj_st_offs(Sym s) {
  ASSERT(lispm_sym_is_st_obj(s));
  return s >> 4;
}
static inline Sym lispm_st_obj_offset_by(Sym s, unsigned offs) {
  /* utility for gc */
  ASSERT(lispm_sym_is_st_obj(s));
  return s + (offs << 4);
}

/* cons */
static inline Sym lispm_make_cons(unsigned st_offs) { return lispm_make_st_obj(LISPM_ST_OBJ_CONS, st_offs); }
static inline int lispm_sym_is_cons(Sym s) { return lispm_st_obj_kind(s) == LISPM_ST_OBJ_CONS; }

/* lambda */
static inline Sym lispm_make_lambda(unsigned st_offs) { return lispm_make_st_obj(LISPM_ST_OBJ_LAMBDA, st_offs); }
static inline int lispm_sym_is_lambda(Sym s) { return lispm_st_obj_kind(s) == LISPM_ST_OBJ_LAMBDA; }

/* spans */
static inline Sym lispm_make_span(unsigned st_offs) { return lispm_make_st_obj(LISPM_ST_OBJ_SPAN, st_offs); }
static inline int lispm_sym_is_span(Sym s) { return lispm_st_obj_kind(s) == LISPM_ST_OBJ_SPAN; }

/* specials, ctors are defined in macros, to be compile time consts */
static inline int lispm_sym_is_special(Sym s) { return (s & 3u) == 3u; }

/* builtin functions */
/* special forms receive their arguments un-evaluated */
#define LISPM_SPECIAL_FORM_BIT         UPPER_BITS(1)
#define LISPM_MAKE_BUILTIN_FN(ft_offs) (((ft_offs) << 4) | 3u)
static inline int lispm_sym_is_builtin_fn(Sym s) { return (s & 15u) == 3u; }
static inline int lispm_sym_is_special_form(Sym s) {
  return (s & (LISPM_SPECIAL_FORM_BIT | 15u)) == (LISPM_SPECIAL_FORM_BIT | 3u);
}
static inline unsigned lispm_builtin_fn_ft_offs(Sym s) {
  ASSERT(lispm_sym_is_builtin_fn(s));
  return (s & ~LISPM_SPECIAL_FORM_BIT) >> 4;
}

/* pages */
#define LISPM_MAKE_PAGE(pt_offs) ((pt_offs << 4) | 7u)
static inline int lispm_sym_is_page(Sym s) { return (s & 15u) == 7u; }
static inline unsigned lispm_page_pt_offs(Sym s) { return s >> 4; }

/* special values */
#define LISPM_MAKE_SPECIAL_VALUE(val) (((val) << 4) | 15u)

#define LISPM_SYM_NIL      0u
#define LISPM_SYM_T        LISPM_MAKE_SPECIAL_VALUE(0)
#define LISPM_SYM_PROGRAM  LISPM_MAKE_SPECIAL_VALUE(1)
#define LISPM_SYM_BINDING  LISPM_MAKE_SPECIAL_VALUE(~0u - 2)
#define LISPM_SYM_CAPTURED LISPM_MAKE_SPECIAL_VALUE(~0u - 1)
#define LISPM_SYM_NO_ASSOC LISPM_MAKE_SPECIAL_VALUE(~0u - 0)

#define LISPM_ERR_INIT  LISPM_MAKE_SPECIAL_VALUE(~0u - 1024)
#define LISPM_ERR_OOM   LISPM_MAKE_SPECIAL_VALUE(~0u - 1025)
#define LISPM_ERR_LEX   LISPM_MAKE_SPECIAL_VALUE(~0u - 1026)
#define LISPM_ERR_PARSE LISPM_MAKE_SPECIAL_VALUE(~0u - 1027)
#define LISPM_ERR_EVAL  LISPM_MAKE_SPECIAL_VALUE(~0u - 1028)
