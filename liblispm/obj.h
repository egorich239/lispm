#pragma once

#include <liblispm/config.h>
#include <liblispm/trace.h>
#include <liblispm/types.h>

/* LispmObj API */
/* nil */
static inline int lispm_obj_is_nil(LispmObj s) { return !s; }

/* literals */
static inline int lispm_obj_is_literal(LispmObj s) { return (s & 3u) == 0; }
static inline LispmObj lispm_make_literal(unsigned ht_offs) { return ht_offs << 2; }
static inline unsigned lispm_literal_ht_offs(LispmObj s) {
  LISPM_ASSERT(lispm_obj_is_literal(s));
  return s >> 2;
}

/* unsigneds */
static inline int lispm_shortnum_can_represent(unsigned val) { return (val & ~(~0u >> 2)) == 0; }
static inline LispmObj lispm_make_shortnum(unsigned val) {
  LISPM_ASSERT(lispm_shortnum_can_represent(val));
  return (val << 2) | 1u;
}
static inline int lispm_obj_is_shortnum(LispmObj s) { return (s & 3u) == 1; }
static inline unsigned lispm_shortnum_val(LispmObj s) {
  LISPM_ASSERT(lispm_obj_is_shortnum(s));
  return s >> 2;
}
static inline unsigned lispm_shortnum_sval(LispmObj s) {
  LISPM_ASSERT(lispm_obj_is_shortnum(s));
  return ((int)s) >> 2;
}
static inline LispmObj lispm_shortnum_bitwise_not(LispmObj p) {
  LISPM_ASSERT(lispm_obj_is_shortnum(p));
  return ~p - 1;
}
static inline LispmObj lispm_shortnum_bitwise_or(LispmObj p, LispmObj q) {
  LISPM_ASSERT(lispm_obj_is_shortnum(p) && lispm_obj_is_shortnum(q));
  return p | q;
}
static inline LispmObj lispm_shortnum_bitwise_xor(LispmObj p, LispmObj q) {
  LISPM_ASSERT(lispm_obj_is_shortnum(p) && lispm_obj_is_shortnum(q));
  return p ^ q ^ 1u;
}
static inline LispmObj lispm_shortnum_bitwise_and(LispmObj p, LispmObj q) {
  LISPM_ASSERT(lispm_obj_is_shortnum(p) && lispm_obj_is_shortnum(q));
  return p & q;
}
static inline LispmObj lispm_shortnum_neg(LispmObj p) {
  LISPM_ASSERT(lispm_obj_is_shortnum(p));
  return ~p + 3; /* (~p + (1 << 2)) - 1*/
}
static inline LispmObj lispm_shortnum_add(LispmObj p, LispmObj q, int *overflow) {
  LISPM_ASSERT(lispm_obj_is_shortnum(p) && lispm_obj_is_shortnum(q));
  LispmObj res;
  *overflow = __builtin_uadd_overflow(p, q, &res);
  return res - 1;
}
static inline LispmObj lispm_shortnum_sub(LispmObj p, LispmObj q, int *overflow) {
  LISPM_ASSERT(lispm_obj_is_shortnum(p) && lispm_obj_is_shortnum(q));
  unsigned res;
  *overflow = __builtin_usub_overflow(lispm_shortnum_val(p), lispm_shortnum_val(q), &res);
  return lispm_make_shortnum(res & (~0u >> 2));
}
static inline LispmObj lispm_shortnum_mul(LispmObj p, LispmObj q, int *overflow) {
  LISPM_ASSERT(lispm_obj_is_shortnum(p) && lispm_obj_is_shortnum(q));
  unsigned res;
  *overflow =
      __builtin_umul_overflow(lispm_shortnum_val(p), lispm_shortnum_val(q), &res) || !lispm_shortnum_can_represent(res);
  return lispm_make_shortnum(res & (~0u >> 2));
}

/* stack objects */
static inline int lispm_obj_is_st_obj(LispmObj s) { return (s & 3u) == 2; }
static inline LispmObj lispm_make_st_obj(enum LispmStObjKind k, unsigned st_offs) {
  LISPM_ASSERT(lispm_obj_is_st_obj(k));
  return (st_offs << 4) | k;
}
static inline unsigned lispm_st_obj_kind(LispmObj s) { return s & 15u; }
static inline unsigned lispm_st_obj_st_size(LispmObj s) {
  LISPM_ASSERT(lispm_obj_is_st_obj(s));
  return ((s >> 2) & 3) + 2;
}
static inline unsigned lispm_st_obj_st_offs(LispmObj s) {
  LISPM_ASSERT(lispm_obj_is_st_obj(s));
  return s >> 4;
}
static inline LispmObj lispm_st_obj_offset_by(LispmObj s, unsigned offs) {
  /* utility for gc */
  LISPM_ASSERT(lispm_obj_is_st_obj(s));
  return s + (offs << 4);
}

/* atoms */
static inline int lispm_obj_is_atom(LispmObj s) { return (s & 2u) == 0; }

/* cons */
static inline LispmObj lispm_make_cons(unsigned st_offs) { return lispm_make_st_obj(LISPM_ST_OBJ_CONS, st_offs); }
static inline int lispm_obj_is_cons(LispmObj s) { return lispm_st_obj_kind(s) == LISPM_ST_OBJ_CONS; }

/* lambda */
static inline LispmObj lispm_make_triplet(unsigned st_offs) { return lispm_make_st_obj(LISPM_ST_OBJ_TRIPLET, st_offs); }
static inline int lispm_obj_is_triplet(LispmObj s) { return lispm_st_obj_kind(s) == LISPM_ST_OBJ_TRIPLET; }
static inline int lispm_obj_is_quad(LispmObj s) { return lispm_st_obj_kind(s) == LISPM_ST_OBJ_QUAD; }
static inline int lispm_obj_is_penta(LispmObj s) { return lispm_st_obj_kind(s) == LISPM_ST_OBJ_PENTA; }

/* specials, ctors are defined in macros, to be compile time consts */
static inline int lispm_obj_is_special(LispmObj s) { return (s & 3u) == 3u; }

/* builtin functions */
#define LISPM_MAKE_BUILTIN_SYM(ft_offs) (((ft_offs) << 8) | 3u)
static inline int lispm_obj_is_builtin_sym(LispmObj s) { return (s & 255u) == 3u; }
static inline unsigned lispm_builtin_sym_offs(LispmObj s) {
  LISPM_ASSERT(lispm_obj_is_builtin_sym(s));
  return s >> 8;
}

/**
 * Returns object, corresponding to a given builtin.
 */
LispmObj lispm_obj_from_builtin(const struct LispmBuiltin *bi);

/**
 * Allocates object of kind `k` on the stack.
 * Terminates the VM if the stack is exhausted.
 */
LispmObj lispm_obj_alloc0(enum LispmStObjKind k);
/**
 * Returns pointer to the first word of a stack object.
 */
LispmObj *lispm_obj_unpack(LispmObj s);

static inline LispmObj lispm_cons_alloc(LispmObj car, LispmObj cdr) {
  LispmObj res = lispm_obj_alloc0(LISPM_ST_OBJ_CONS);
  lispm.sp[0] = cdr, lispm.sp[1] = car;
  return res;
}
static inline LispmObj lispm_triplet_alloc(LispmObj a, LispmObj b, LispmObj n) {
  LispmObj res = lispm_obj_alloc0(LISPM_ST_OBJ_TRIPLET);
  lispm.sp[0] = n, lispm.sp[1] = a, lispm.sp[2] = b;
  return res;
}
static inline LispmObj lispm_quad_alloc(LispmObj a, LispmObj b, LispmObj c, LispmObj n) {
  LispmObj res = lispm_obj_alloc0(LISPM_ST_OBJ_QUAD);
  lispm.sp[0] = n, lispm.sp[1] = a, lispm.sp[2] = b, lispm.sp[3] = c;
  return res;
}
static inline LispmObj lispm_penta_alloc(LispmObj a, LispmObj b, LispmObj c, LispmObj d, LispmObj n) {
  LispmObj res = lispm_obj_alloc0(LISPM_ST_OBJ_PENTA);
  lispm.sp[0] = n, lispm.sp[1] = a, lispm.sp[2] = b, lispm.sp[3] = c, lispm.sp[4] = d;
  return res;
}
