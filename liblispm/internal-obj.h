#pragma once

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