#pragma once

#include "lispm-builtins.h"
#include "lispm-obj.h"
#include "lispm-rt.h"
#include "lispm-trace.h"
#include "lispm-types.h"

enum {
  /* The bottom of the stack is used to communicate information about errors. */
  LISPM_STACK_BOTTOM_OFFSET = 8u,
};

/* External symbols that runtime must provide: */
/* Machine must be initialized before calling lispm_exec(). */
extern struct Lispm lispm;

#define LISPM_UPPER_BITS(n) ~(~0u >> (n))

/* API */
static inline int lispm_is_power_of_two(unsigned i) { return i && !(i & (i - 1)); }
static inline int lispm_is_valid_config(void) {
  const struct Lispm *m = &lispm;
  return m->stack + LISPM_STACK_BOTTOM_OFFSET < m->stack_end /**/
         && m->strings + 8 <= m->strings_end                 /**/
         && m->program <= m->pc && m->pc < m->program_end    /**/
         && m->htable + 1024 <= m->htable_end                /**/
         && lispm_is_power_of_two(m->htable_end - m->htable)
         /* we also want all offsets to fit into short unsigned */
         && lispm_shortnum_can_represent(m->stack_end - m->stack)     /**/
         && lispm_shortnum_can_represent(m->strings_end - m->strings) /**/
         && lispm_shortnum_can_represent(m->program_end - m->program) /**/
         && lispm_shortnum_can_represent(m->htable_end - m->htable);
}

/* Runs lispm and returns the evaluated symbol */
void lispm_init(void);
Sym lispm_exec(void);

static inline unsigned lispm_literal_str_offs(Sym s) {
  LISPM_ASSERT(lispm_sym_is_literal(s));
  return lispm.htable[lispm_literal_ht_offs(s)] >> 2;
}

/* Internal API */
__attribute__((noreturn)) void lispm_panic(Sym ctx);

/* pc must be between M.program and M.program_end */
Sym lispm_parse_quote(const char *pc, const char *pc_end);
Sym lispm_eval(const char *pc, const char *pc_end);

unsigned lispm_list_scan(Sym *out, Sym li, unsigned limit);

Sym lispm_st_obj_alloc(unsigned k);
Sym *lispm_st_obj_unpack(Sym s);

static inline Sym lispm_cons_alloc(Sym car, Sym cdr) {
  Sym res = lispm_st_obj_alloc(LISPM_ST_OBJ_CONS);
  lispm.sp[0] = car, lispm.sp[1] = cdr;
  return res;
}
static inline Sym lispm_triplet_alloc(Sym a, Sym b, Sym n) {
  Sym res = lispm_st_obj_alloc(LISPM_ST_OBJ_TRIPLET);
  lispm.sp[0] = a, lispm.sp[1] = b, lispm.sp[2] = n;
  return res;
}

Sym lispm_sym_from_builtin(const struct Builtin *bi);
