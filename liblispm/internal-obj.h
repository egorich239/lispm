#pragma once

#include "lispm.h"
#include <liblispm/obj.h>
#include <liblispm/trace.h>
#include <liblispm/types.h>

/* builtin functions and symbols */
static inline LispmObj lispm_make_builtin(unsigned ft_offs) { return (ft_offs << 8) | 3u; }
static inline int lispm_obj_is_builtin(LispmObj s) { return (s & 255u) == 3u; }
static inline unsigned lispm_obj_builtin_offs(LispmObj s) {
  LISPM_ASSERT(lispm_obj_is_builtin(s));
  return s >> 8;
}

/* lexer */
static inline LispmObj lispm_make_token(char c) { return (((unsigned char)c) << 8) | 19u; }
enum {
  LISPM_TOK_LPAREN = (((unsigned char)'(') << 8) | 19u,
  LISPM_TOK_RPAREN = (((unsigned char)')') << 8) | 19u,
  LISPM_TOK_QUOTE = (((unsigned char)'\'') << 8) | 19u,
};

/* hash table */
enum {
  /* symbols */
  LISPM_HTABLE_EXHAUSTED = (0u << 8) | 35u,
  LISPM_HTABLE_NOT_FOUND = (1u << 8) | 35u,

  /* htable_ensure flags */
  LISPM_HTABLE_ENSURE_FORBID_INSERT = 4u,
};
static inline int lispm_obj_is_htable_error(LispmObj o) { return (o & 255u) == 35u; }

/* frame depth handling */
static inline int lispm_is_frame_depth(LispmObj o) {
  return lispm_obj_is_shortnum(o) && 0 < lispm_shortnum_val(o) && lispm_shortnum_val(o) < ~(~0u >> 4);
}
static inline void lispm_frame_depth_inc(LispmObj *frame_depth) {
  LISPM_ASSERT(lispm_is_frame_depth(*frame_depth) && lispm_is_frame_depth(*frame_depth + 4));
  *frame_depth += 4;
}
static inline void lispm_frame_depth_dec(LispmObj *frame_depth) {
  LISPM_ASSERT(lispm_is_frame_depth(*frame_depth) && lispm_is_frame_depth(*frame_depth - 4));
  *frame_depth -= 4;
}

/* semantic analysis */
enum {
  LISPM_LEX_UNBOUND = 15u,

  LISPM_LEX_FREE = 11,
  LISPM_LEX_BOUND = 15,
  LISPM_LEX_BOUNDREC = 15 | ~(~0u >> 1),
};
static inline LispmObj lispm_make_sema(unsigned frame_depth, unsigned mask) {
  LISPM_ASSERT((frame_depth < ~(~0u >> 5)) &&
               (mask == LISPM_LEX_BOUND || mask == LISPM_LEX_BOUNDREC || mask == LISPM_LEX_FREE));
  return (frame_depth << 4) | mask;
}

static inline int lispm_obj_is_sema(LispmObj o) { return (o & 11u) == 11u; }
static inline int lispm_obj_is_sema_bound(LispmObj o) { return (o & 15u) == LISPM_LEX_BOUND; }
static inline int lispm_obj_is_sema_free(LispmObj o) { return (o & 15u) == LISPM_LEX_FREE; }
static inline unsigned lispm_obj_sema_depth(LispmObj o) {
  LISPM_ASSERT(lispm_obj_is_sema(o));
  return (o & (~0u >> 1)) >> 4;
}

/* evaluation phase */
enum { LISPM_OBJ_ASSOC_MASK = 15 };
static inline LispmObj lispm_make_assoc(LispmObj p) {
  LISPM_ASSERT(lispm_obj_is_penta(p));
  return p ^ 1;
}
static inline int lispm_obj_is_assoc(LispmObj o) { return (o & 15u) == LISPM_OBJ_ASSOC_MASK; }
static inline int lispm_obj_assoc_deref(LispmObj o) {
  LISPM_ASSERT(lispm_obj_is_assoc(o));
  return o ^ 1;
}

/* gc */
static inline LispmObj lispm_gc_bound(unsigned mark) { return lispm_make_st_obj(LISPM_ST_OBJ_CONS, mark); }
static inline unsigned lispm_gc_offset(unsigned low_mark, unsigned high_mark) {
  /* must be in sync with the representation of stack objects,
     and be equal to `lispm_gc_bound(CONS, high_mark) - lispm_gc_bound(CONS, low_mark)` */
  return (high_mark - low_mark) << 4;
}
static inline LispmObj lispm_gc_move(LispmObj s, unsigned gc_offset) {
  /* utility for gc */
  LISPM_ASSERT(lispm_obj_is_nil(s) || lispm_obj_is_st_obj(s));
  return !lispm_obj_is_nil(s) ? s + gc_offset : s;
}
