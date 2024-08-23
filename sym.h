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
 * -    <PTR> 11 10: stack position of the triplet page (PTR), offs (PTR+1),
 *                   length of memory range;
 *                   page is the page symbol (see below),
 *                   offs is byte offset at this page, encoded as numerical;
 *                   memory range encodes the range as numerical.
 *
 * The 11 trailing bits are reserved for special symbols;
 * all of them have upper bit zero, which is used by hash table to
 * mark the literals that cannot be overriden by a locally bound variable.
 *
 * - 0 F <OFFS> 00 11: builtin function at the specific offset in the ftable;
 *                     when F bit is 1, it denotes a special form;
 *                     special forms receive their params unevaluated.
 * - 0   <OFFS> 01 11: page, offs specifies offset in page table;
 *
 * - 0      ... 11 11: special values (assuming 32-bit unsigned)
 *          0000 000F: T
 *          7FFF FFDF: used during evaluation of capture lists of LET;
 *                   marks a literal first defined in the previous part of
 *                   the LET evaluations;
 *          7FFF FFEF: used during evaluation of capture lists of LAMBDA/LET;
 *                   marks a literal already in the capture list;
 *          7FFF FFFF: no value currently associated with the literal.
 */
typedef unsigned Sym;

#define UPPER_BITS(n) ~(~0u >> (n))

/* atoms */
static inline int is_nil(Sym s) { return !s; }
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
static inline Sym unsigned_sub(Sym a, Sym b, int *oflow) {
  Sym res;
  *oflow = __builtin_usub_overflow(a, b, &res);
  return res | 1u;
}
static inline Sym unsigned_mul(Sym a, Sym b, int *oflow) {
  Sym res;
  *oflow = __builtin_umul_overflow(unsigned_val(a), unsigned_val(b), &res) |
           (res & UPPER_BITS(2));
  return make_unsigned(res);
}

/* stack objects*/
#define ST_OBJ_CONS    2u
#define ST_OBJ_LAMBDA  6u
#define ST_OBJ_POINTER 14u
static inline Sym make_st_obj(unsigned k, unsigned st_offs) {
  ASSERT(k == ST_OBJ_CONS || k == ST_OBJ_LAMBDA || k == ST_OBJ_POINTER);
  return (st_offs << 4) | k;
}
static inline int is_st_obj(Sym s) { return (s & 3u) == 2; }
static inline unsigned st_obj_kind(Sym s) { return s & 15u; }
static inline unsigned st_obj_st_size(Sym s) {
  /* size of the object on the stack, in words */
  return (s & 4) ? 3 : 2; /* ((s&4) >> 2) + 2 ?? */
}
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
static inline Sym make_cons(unsigned st_offs) {
  return make_st_obj(ST_OBJ_CONS, st_offs);
}
static inline int is_cons(Sym s) { return st_obj_kind(s) == ST_OBJ_CONS; }

/* lambda */
static inline Sym make_lambda(unsigned st_offs) {
  return make_st_obj(ST_OBJ_LAMBDA, st_offs);
}
static inline int is_lambda(Sym s) { return st_obj_kind(s) == ST_OBJ_LAMBDA; }

/* pointer */
static inline Sym make_pointer(unsigned st_offs) {
  return make_st_obj(ST_OBJ_POINTER, st_offs);
}
static inline int is_pointer(Sym s) { return st_obj_kind(s) == ST_OBJ_POINTER; }

/* specials, ctors are defined in macros, to be compile time consts */
#define SPECIAL_READONLY_BIT UPPER_BITS(1)
static inline int is_special(Sym s) { return (s & 3u) == 3u; }
static inline int special_is_readonly(Sym s) {
  ASSERT(is_special(s));
  return s & SPECIAL_READONLY_BIT;
}

/* builtin functions */
/* special forms receive their arguments un-evaluated */
#define SPECIAL_FORM_BIT         (UPPER_BITS(1) >> 1)
#define MAKE_BUILTIN_FN(ft_offs) (((ft_offs) << 4) | 3u)
static inline int is_builtin_fn(Sym s) { return (s & 15u) == 3u; }
static inline int is_special_form(Sym s) {
  return (s & (SPECIAL_FORM_BIT | 15u)) == (SPECIAL_FORM_BIT | 3u);
}
static inline unsigned builtin_fn_ft_offs(Sym s) {
  ASSERT(is_builtin_fn(s));
  return (s & ~(SPECIAL_READONLY_BIT | SPECIAL_FORM_BIT)) >> 4;
}

/* pages */
#define MAKE_PAGE(pt_offs) ((pt_offs << 4) | 7u)
static inline int is_page(Sym s) { return (s & 15u) == 7u; }
static inline unsigned page_pt_offs(Sym s) { return s >> 4; }

/* special values */
#define MAKE_SPECIAL_VALUE(val) ((((val) << 4) | 15u) & ~SPECIAL_READONLY_BIT)

#define SYM_NIL      0u
#define SYM_T        MAKE_SPECIAL_VALUE(0)
#define SYM_PROGRAM  MAKE_SPECIAL_VALUE(1)
#define SYM_BINDING  MAKE_SPECIAL_VALUE(~0u - 2)
#define SYM_CAPTURED MAKE_SPECIAL_VALUE(~0u - 1)
#define SYM_NO_ASSOC MAKE_SPECIAL_VALUE(~0u - 0)

#define ERR_INIT  MAKE_SPECIAL_VALUE(~0u - 1024)
#define ERR_OOM   MAKE_SPECIAL_VALUE(~0u - 1025)
#define ERR_LEX   MAKE_SPECIAL_VALUE(~0u - 1026)
#define ERR_PARSE MAKE_SPECIAL_VALUE(~0u - 1027)
#define ERR_EVAL  MAKE_SPECIAL_VALUE(~0u - 1028)
