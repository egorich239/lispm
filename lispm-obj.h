#pragma once

#include "lispm-config.h"
#include "lispm-trace.h"
#include "lispm-types.h"

/* Sym API */
/* nil */
static inline int lispm_sym_is_nil(Sym s) { return !s; }

/* literals */
static inline Sym lispm_make_literal(unsigned ht_offs) { return ht_offs << 2; }
static inline int lispm_sym_is_literal(Sym s) { return (s & 3u) == 0; }
static inline unsigned lispm_literal_ht_offs(Sym s) {
  LISPM_ASSERT(lispm_sym_is_literal(s));
  return s >> 2;
}

/* unsigneds */
static inline int lispm_shortnum_can_represent(unsigned val) { return (val & ~(~0u >> 2)) == 0; }
static inline Sym lispm_make_shortnum(unsigned val) {
  LISPM_ASSERT(lispm_shortnum_can_represent(val));
  return (val << 2) | 1u;
}
static inline int lispm_sym_is_shortnum(Sym s) { return (s & 3u) == 1; }
static inline unsigned lispm_shortnum_val(Sym s) {
  LISPM_ASSERT(lispm_sym_is_shortnum(s));
  return s >> 2;
}
static inline unsigned lispm_shortnum_sval(Sym s) {
  LISPM_ASSERT(lispm_sym_is_shortnum(s));
  return ((int)s) >> 2;
}
static inline Sym lispm_shortnum_bitwise_not(Sym p) {
  LISPM_ASSERT(lispm_sym_is_shortnum(p));
  return ~p - 1;
}
static inline Sym lispm_shortnum_bitwise_or(Sym p, Sym q) {
  LISPM_ASSERT(lispm_sym_is_shortnum(p) && lispm_sym_is_shortnum(q));
  return p | q;
}
static inline Sym lispm_shortnum_bitwise_xor(Sym p, Sym q) {
  LISPM_ASSERT(lispm_sym_is_shortnum(p) && lispm_sym_is_shortnum(q));
  return p ^ q ^ 1u;
}
static inline Sym lispm_shortnum_bitwise_and(Sym p, Sym q) {
  LISPM_ASSERT(lispm_sym_is_shortnum(p) && lispm_sym_is_shortnum(q));
  return p & q;
}
static inline Sym lispm_shortnum_neg(Sym p) {
  LISPM_ASSERT(lispm_sym_is_shortnum(p));
  return ~p + 3; /* (~p + (1 << 2)) - 1*/
}
static inline Sym lispm_shortnum_add(Sym p, Sym q, int *overflow) {
  LISPM_ASSERT(lispm_sym_is_shortnum(p) && lispm_sym_is_shortnum(q));
  Sym res;
  *overflow = __builtin_uadd_overflow(p, q, &res);
  return res - 1;
}
static inline Sym lispm_shortnum_sub(Sym p, Sym q, int *overflow) {
  LISPM_ASSERT(lispm_sym_is_shortnum(p) && lispm_sym_is_shortnum(q));
  unsigned res;
  *overflow = __builtin_usub_overflow(lispm_shortnum_val(p), lispm_shortnum_val(q), &res);
  return lispm_make_shortnum(res & (~0u >> 2));
}
static inline Sym lispm_shortnum_mul(Sym p, Sym q, int *overflow) {
  LISPM_ASSERT(lispm_sym_is_shortnum(p) && lispm_sym_is_shortnum(q));
  unsigned res;
  *overflow =
      __builtin_umul_overflow(lispm_shortnum_val(p), lispm_shortnum_val(q), &res) || !lispm_shortnum_can_represent(res);
  return lispm_make_shortnum(res & (~0u >> 2));
}

/* stack objects */
enum {
  LISPM_ST_OBJ_CONS = 2u,
  LISPM_ST_OBJ_LAMBDA = 6u,

  /* an extension object, taking TWO words on stack */
  LISPM_ST_OBJ_EXT_2 = 10u,

  /* an extension object, taking THREE words on stack */
  LISPM_ST_OBJ_EXT_3 = 14u,
};
static inline int lispm_sym_is_st_obj(Sym s) { return (s & 3u) == 2; }
static inline Sym lispm_make_st_obj(unsigned k, unsigned st_offs) {
  LISPM_ASSERT(lispm_sym_is_st_obj(k));
  return (st_offs << 4) | k;
}
static inline unsigned lispm_st_obj_kind(Sym s) { return s & 15u; }
static inline unsigned lispm_st_obj_st_size(Sym s) {
  LISPM_ASSERT(lispm_sym_is_st_obj(s));
  /* size of the object on the stack, in words */
  return (s & 4) ? 3 : 2; /* ((s&4) >> 2) + 2 ?? */
}
static inline unsigned lispm_st_obj_st_offs(Sym s) {
  LISPM_ASSERT(lispm_sym_is_st_obj(s));
  return s >> 4;
}
static inline Sym lispm_st_obj_offset_by(Sym s, unsigned offs) {
  /* utility for gc */
  LISPM_ASSERT(lispm_sym_is_st_obj(s));
  return s + (offs << 4);
}

/* atoms */
static inline int lispm_sym_is_atom(Sym s) { return (s & 2u) == 0; }

/* cons */
static inline Sym lispm_make_cons(unsigned st_offs) { return lispm_make_st_obj(LISPM_ST_OBJ_CONS, st_offs); }
static inline int lispm_sym_is_cons(Sym s) { return lispm_st_obj_kind(s) == LISPM_ST_OBJ_CONS; }

/* lambda */
static inline Sym lispm_make_lambda(unsigned st_offs) { return lispm_make_st_obj(LISPM_ST_OBJ_LAMBDA, st_offs); }
static inline int lispm_sym_is_lambda(Sym s) { return lispm_st_obj_kind(s) == LISPM_ST_OBJ_LAMBDA; }

/* specials, ctors are defined in macros, to be compile time consts */
static inline int lispm_sym_is_special(Sym s) { return (s & 3u) == 3u; }

/* builtin functions */
#define LISPM_MAKE_BUILTIN_SYM(ft_offs) (((ft_offs) << 8) | 3u)
static inline int lispm_sym_is_builtin_sym(Sym s) { return (s & 255u) == 3u; }
static inline unsigned lispm_builtin_sym_offs(Sym s) {
  LISPM_ASSERT(lispm_sym_is_builtin_sym(s));
  return s >> 8;
}
