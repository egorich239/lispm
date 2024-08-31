#pragma once

#ifndef LISPM_CONFIG_ASSERT
#define LISPM_CONFIG_ASSERT 1
#endif
#ifndef LISPM_CONFIG_VERBOSE
#define LISPM_CONFIG_VERBOSE 1
#endif

/* Abort is an external symbol provided by runtime */
extern __attribute__((noreturn)) void lispm_rt_abort(void);

#if LISPM_CONFIG_ASSERT
#define LISPM_ASSERT(cond)                                                                                             \
  do {                                                                                                                 \
    if (!(cond)) lispm_rt_abort();                                                                                     \
  } while (0)
#else
#define LISPM_ASSERT(cond) ((void)(0))
#endif

/** Symbol, one of:
 *  - NIL:            corresponds to empty list
 *  - short unsigned: an atom, corresponding to an unsigned integer
 *                    shorter than native unsigned by two bits;
 *  - literal:        an atom, corresponding to a valid literal;
 *  - stack object:   CONS, LAMBDA, full size unsigned;
 *  - special symbol: T, builtins, errors, implementation values.
 *
 * The general layout takes some lower and upper bits for special information,
 * we describe it for 32-bit unsigned, but same goes for 64-bit unsigned,
 * only the payload fields get wider.
 *
 * Layout:
 * -               0: LISPM_SYM_NIL
 * -    <OFFS:30> 00: literal with the given <OFFS> in htable;
 *                    each literal consumes two words in htable:
 *                    0: b0 <LIT:30> - bit 'b' is 0 for builtin literals, else 1;
 *                                     LIT is offset of NUL-terminated representation
 *                                     of literal in strings table.
 *                    1: the currently assigned value of the literal.
 * -     <NUM:30> 01: short inline unsigned;
 * - <OFFS:28> 0s 10: stack pointer, the object consume '2+s' consequtive words on stack,
 *                    starting with <OFFS>;
 *   Stack objects:
 * -    <CONS> 00 10: pair car, cdr;
 * -  <LAMBDA> 01 10: triplet captures, args, body;
 * -    <SPTR> 10 10: an extension stack object, consisting of exactly TWO words starting
 *                    at SPTR position;
 * -    <SPTR> 11 10: an extension stack object, consisting of exactly THREE words starting
 *                    at SPTR position;
 *
 * Extensions must adhere to the symbol layout in the part of the stack they use
 * (i.e they must put valid symbols on the stack), because garbage collector assumes that
 * everything it ever observes on the stack is some kind of Sym.
 *
 *   Special symbols.
 *
 * -    <N:28> 00 11: builtin function at the offset N in builtins table;
 *
 * -       ... 11 11: special values
 *         0000 000F: T
 *         FFFF FFDF: used during evaluation of capture lists of LET;
 *                    marks a literal first defined in the previous part of
 *                    the LET evaluations;
 *         FFFF FFEF: used during evaluation of capture lists of LAMBDA/LET;
 *                    marks a literal already in the capture list;
 *         FFFF FFFF: no value currently associated with the literal.
 */
typedef unsigned Sym;

/*
 * Arguments are provided as a SYM_NIL-terminated CONS-sequence.
 *
 * If evcap is non-zero, the builtin is treated as special form, i.e. it receives its args unevaluated.
 * This callback is used to evaluate the list of args captured by this special form.
 *
 * We pack builtins into special sections, and rely on linker script to organize them into a single
 * contigous array, and as sections are 16-bytes aligned, we align the struct similarly to avoid
 * mishaps at concatenation of various sections.
 */
struct __attribute__((aligned(16))) Builtin {
  const char *name;
  Sym (*eval)(Sym args);
  Sym (*evcap)(Sym args, Sym caps);
  Sym *store; /* if non-NULL, the registered literal is stored into this location */
};
/* Convenience macro to implement extensions */
#define LISPM_BUILTINS_EXT(name)                                                                                       \
  static const struct Builtin name[] __attribute__((section(".lispm.rodata.builtins.ext"), aligned(16), used))

struct LispmTraceCallbacks {
  void (*apply_enter)(Sym fn, Sym fn_resolved, Sym args);
  void (*apply_leave)(void);
  void (*lambda_cons)(Sym lambda);
  void (*panic)(const char *file, unsigned line, const char *msg, Sym ctx);
};

/* State of LISPM */
struct Lispm {
  /* Array of builtins, terminates with an entry with NULL `name`. */
  const struct Builtin *builtins;

  /* Location of the stack bottom. */
  Sym *stack;
  /* Stack pointer. Grows down. */
  Sym *sp;
  /* Parser pointer. Grows up, must be at least 4 words above stack bottom. */
  Sym *pp;

  /* Beginning of the strings storage. */
  char *strings;
  /* Pointer past the end of strings storage. */
  char *strings_end;
  /* Pointer past the end of the used part of string storage.
     Initialized by lispm_init(). */
  char *tp;

  /* Beginning of the program page. */
  const char *program;
  /* Pointer past the end of the program page. */
  const char *program_end;
  /* Pointer to the next lexeme. */
  const char *pc;

  /* Beginning of hash table */
  unsigned *htable;
  /* Pointer past the end of hash_table, must contain a power-of-two number of words. */
  unsigned *htable_end;

  /* Internal values */
  unsigned htable_index_size;
  int htable_index_shift;

#if LISPM_CONFIG_VERBOSE
  struct LispmTraceCallbacks trace;
#endif
};

/* The very bottom of the stack can be used for special purposes.
   This value defines the size of that bottom part. */
#define LISPM_PP_OFFSET 64u

/* Hash table uses open addressing.
   This value limits how many slots are looked up before we give up. */
#define STRINGS_INDEX_LOOKUP_LIMIT 32u

/* External symbols that runtime must provide: */
/* Invokes `fn` such that a call to `lispm_rt_throw()` causes immediate stack unwinding
   and return from the `lispm_rt_try` function. The library invokes this function exactly
   once. */
extern void lispm_rt_try(void (*fn)(void));
/* Causes immediate return from the `lispm_rt_try()` call.
   Guaranteed to be called only from within `lispm_rt_try` function. */
extern __attribute__((noreturn)) void lispm_rt_throw(void);
/* Machine must be initialized before calling lispm_exec(). */
extern struct Lispm lispm;

#define LISPM_UPPER_BITS(n) ~(~0u >> (n))

/* API */
static inline int lispm_is_power_of_two(unsigned i) { return i && !(i & (i - 1)); }
static inline int lispm_shortnum_can_represent(unsigned val) { return (val & LISPM_UPPER_BITS(2)) == 0; }
static inline int lispm_is_valid_config(void) {
  const struct Lispm *m = &lispm;
  return m->stack + LISPM_PP_OFFSET <= m->pp && m->pp < m->sp /**/
         && m->strings + 8 <= m->strings_end                  /**/
         && m->program <= m->pc && m->pc < m->program_end     /**/
         && m->htable + 1024 <= m->htable_end                 /**/
         && lispm_is_power_of_two(m->htable_end - m->htable)
         /* we also want all offsets to fit into short unsigned */
         && lispm_shortnum_can_represent(m->sp - m->stack)            /**/
         && lispm_shortnum_can_represent(m->strings_end - m->strings) /**/
         && lispm_shortnum_can_represent(m->program_end - m->program) /**/
         && lispm_shortnum_can_represent(m->htable_end - m->htable);
}

/* Runs lispm and returns the evaluated symbol */
void lispm_init(void);
Sym lispm_exec(void);

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
static inline unsigned lispm_literal_str_offs(Sym s) {
  LISPM_ASSERT(lispm_sym_is_literal(s));
  return lispm.htable[lispm_literal_ht_offs(s)] & ~LISPM_UPPER_BITS(1); /* TODO: name these upper bits */
}

/* unsigneds */
#define LISPM_SHORTNUM_BITS (sizeof(Sym) * 8 - 2)
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
  return lispm_make_shortnum(res & ~LISPM_UPPER_BITS(2));
}
static inline Sym lispm_shortnum_mul(Sym p, Sym q, int *overflow) {
  LISPM_ASSERT(lispm_sym_is_shortnum(p) && lispm_sym_is_shortnum(q));
  unsigned res;
  *overflow =
      __builtin_umul_overflow(lispm_shortnum_val(p), lispm_shortnum_val(q), &res) || !lispm_shortnum_can_represent(res);
  return lispm_make_shortnum(res & ~LISPM_UPPER_BITS(2));
}

/* stack objects */
enum {
  LISPM_ST_OBJ_CONS   = 2u,
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
#define LISPM_MAKE_BUILTIN_SYM(ft_offs) (((ft_offs) << 4) | 3u)
static inline int lispm_sym_is_builtin_sym(Sym s) { return (s & 15u) == 3u; }
static inline unsigned lispm_builtin_sym_offs(Sym s) {
  LISPM_ASSERT(lispm_sym_is_builtin_sym(s));
  return s >> 4;
}
Sym lispm_builtin_as_sym(const struct Builtin *bi);

/* special values */
#define LISPM_MAKE_SPECIAL_VALUE(val) (((val) << 4) | 15u)

enum {
  LISPM_SYM_NIL = 0,
  LISPM_SYM_T   = LISPM_MAKE_BUILTIN_SYM(0),

  LISPM_SYM_NO_ASSOC = LISPM_MAKE_SPECIAL_VALUE(0),
  LISPM_SYM_BOUND    = LISPM_MAKE_SPECIAL_VALUE(1),
  LISPM_SYM_FREE     = LISPM_MAKE_SPECIAL_VALUE(2),

  LISPM_ERR_OOM   = LISPM_MAKE_SPECIAL_VALUE(1024 + 0),
  LISPM_ERR_LEX   = LISPM_MAKE_SPECIAL_VALUE(1024 + 1),
  LISPM_ERR_PARSE = LISPM_MAKE_SPECIAL_VALUE(1024 + 2),
  LISPM_ERR_EVAL  = LISPM_MAKE_SPECIAL_VALUE(1024 + 3),
};

static inline int lispm_sym_is_error(Sym s) {
  return lispm_sym_is_special(s) && LISPM_ERR_OOM <= s && s <= LISPM_ERR_EVAL;
}

/* Internal API */
#if LISPM_CONFIG_VERBOSE
#define LISPM_TRACE(event, ...)                                                                                        \
  do {                                                                                                                 \
    if (M.trace.event) M.trace.event(__VA_ARGS__);                                                                     \
  } while (0)
#else
#define LISPM_TRACE(...) ((void)0)
#endif

/* Unlike LISPM_ASSERT, these errors are caused by a bug in the user code. */
#define LISPM_EVAL_CHECK(cond, err, diag, ctx)                                                                         \
  do {                                                                                                                 \
    if (!(cond)) {                                                                                                     \
      LISPM_TRACE(panic, __FILE__, __LINE__, diag, ctx);                                                               \
      lispm_report_error(err, ctx);                                                                                    \
    }                                                                                                                  \
  } while (0)

__attribute__((noreturn)) void lispm_report_error(Sym err, Sym ctx);

/* pc must be between M.program and M.program_end */
Sym lispm_parse(const char *pc, const char *pc_end);
Sym lispm_eval(const char *pc, const char *pc_end);

Sym lispm_st_obj_alloc(unsigned k, Sym *vals);
static inline Sym lispm_cons_alloc(Sym car, Sym cdr) {
  Sym cons[2] = {car, cdr};
  return lispm_st_obj_alloc(LISPM_ST_OBJ_CONS, cons);
}

Sym *lispm_st_obj_unpack(Sym s);
Sym *lispm_cons_unpack_user(Sym a);

Sym lispm_evcap_quote(Sym a, Sym c);
Sym lispm_evquote(Sym a);
void lispm_args_unpack2(Sym a, Sym *f, Sym *s);
