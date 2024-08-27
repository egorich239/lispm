#pragma once

#define LISPM_CONFIG_ASSERT  0
#define LISPM_CONFIG_VERBOSE 0

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

/* Symbol: last two bits encode its kind
 * - 0    ...    00: LISPM_SYM_NIL
 * - 0    <OFFS> 00: builtin symbol in htable, key pointer at '<OFFS>';
 * - 1    <OFFS> 00: user-defined symbol in htable, key pointer at '<OFFS>';
 * -       <NUM> 01: short inline unsigned;
 * -   <CONS> 00 10: stack position of the pair car (CONS), cdr (CONS+1);
 * - <LAMBDA> 01 10: stack position of the triplet captures (LAMBDA), args (+1),
 *                   body (+2);
 * -   <WORD> 10 10: stack position of the pair, with first element representing
 *                   the high bits of the word, and second element
 *                   either representing the lower bits, or another `<WORD> 10 10`
 *                   for an even longer word;
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

/* Page access from the perspective of a LISP program. */
enum PageAccessFlags {
  /* The page is writable */
  PAGE_ACCESS_W = 2,
  /* The page contains native platform code */
  PAGE_ACCESS_X = 4,
};

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
};
/* Convenience macro to implement extensions */
#define LISPM_BUILTINS_EXT(name)                                                                                       \
  static const struct Builtin name[] __attribute__((section(".lispm.rodata.builtins.ext"), used))

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
  /* Pointer past the end of the used part of string storage. */
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
};

/* The very bottom of the stack can be used for special purposes.
   This value defines the size of that bottom part. */
#define LISPM_PP_OFFSET 64u

/* The initial part of the strings table contains the error message string. */
#define LISPM_DIAG_SIZE 256u
_Static_assert(LISPM_DIAG_SIZE > 0, "error message size must be non-zero");

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
/* Callback to get the properties of a page.
   IDs 0 and 1 are reserved for program and strings pages, this function is never called with these ids. */
extern void lispm_rt_page(unsigned id, void **begin, void **end, unsigned *page_access);
/* Machine must be initialized before calling lispm_exec(). */
extern struct Lispm lispm;

/* API */
static inline int lispm_is_power_of_two(unsigned i) { return i && !(i & (i - 1)); }
static inline int lispm_is_valid_config(void) {
  const struct Lispm *m = &lispm;
  return m->stack + LISPM_PP_OFFSET <= m->pp && m->pp < m->sp               /**/
         && m->strings + LISPM_DIAG_SIZE <= m->tp && m->tp < m->strings_end /**/
         && m->program <= m->pc && m->pc < m->program_end                   /**/
         && m->htable + 1024 <= m->htable_end && lispm_is_power_of_two(m->htable_end - m->htable);
}
Sym lispm_exec(void);

/* Sym API */
#define UPPER_BITS(n) ~(~0u >> (n))

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
  return lispm.htable[lispm_literal_ht_offs(s)] & ~UPPER_BITS(1); /* TODO: name these upper bits */
}

/* unsigneds */
#define LISPM_SHORTNUM_BITS (sizeof(Sym) * 8 - 2)
static inline int lispm_shortnum_can_represent(unsigned val) { return (val & UPPER_BITS(2)) == 0; }
static inline Sym lispm_make_shortnum(unsigned val) {
  LISPM_ASSERT(lispm_shortnum_can_represent(val));
  return (val << 2) | 1u;
}
static inline int lispm_sym_is_shortnum(Sym s) { return (s & 3u) == 1; }
static inline unsigned lispm_shortnum_val(Sym s) {
  LISPM_ASSERT(lispm_sym_is_shortnum(s));
  return s >> 2;
}

/* stack objects */
#define LISPM_ST_OBJ_CONS    2u
#define LISPM_ST_OBJ_LAMBDA  6u
#define LISPM_ST_OBJ_LONGNUM 10u
#define LISPM_ST_OBJ_SPAN    14u
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
static inline int lispm_sym_is_atom(Sym s) { return (s & 2u) == 0 || (s & 15u) == LISPM_ST_OBJ_LONGNUM; }

/* cons */
static inline Sym lispm_make_cons(unsigned st_offs) { return lispm_make_st_obj(LISPM_ST_OBJ_CONS, st_offs); }
static inline int lispm_sym_is_cons(Sym s) { return lispm_st_obj_kind(s) == LISPM_ST_OBJ_CONS; }

/* lambda */
static inline Sym lispm_make_lambda(unsigned st_offs) { return lispm_make_st_obj(LISPM_ST_OBJ_LAMBDA, st_offs); }
static inline int lispm_sym_is_lambda(Sym s) { return lispm_st_obj_kind(s) == LISPM_ST_OBJ_LAMBDA; }

/* longnum (2 or more words, similar to cons) */
static inline Sym lispm_make_longnum(unsigned st_offs) { return lispm_make_st_obj(LISPM_ST_OBJ_LONGNUM, st_offs); }
static inline int lispm_sym_is_longnum(Sym s) { return lispm_st_obj_kind(s) == LISPM_ST_OBJ_LONGNUM; }

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
  LISPM_ASSERT(lispm_sym_is_builtin_fn(s));
  return (s & ~LISPM_SPECIAL_FORM_BIT) >> 4;
}

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

/* Internal API */
static inline void lispm_error_message_set(const char *msg) {
  int i = 0;
  while (i + 1 < LISPM_DIAG_SIZE && (lispm.strings[i++] = *msg++)) {}
}

/* Unlike LISPM_ASSERT, these errors are caused by a bug in the user code. */
#if !LISPM_CONFIG_VERBOSE
#define LISPM_EVAL_CHECK(cond, err)                                                                                    \
  do {                                                                                                                 \
    if (!(cond)) lispm_report_error(err);                                                                              \
  } while (0)
#else
#define LISPM_EVAL_CHECK(cond, err)                                                                                    \
  do {                                                                                                                 \
    if (!(cond)) {                                                                                                     \
      lispm_error_message_set("Failed assertion: " #cond);                                                             \
      lispm_report_error(err);                                                                                         \
    }                                                                                                                  \
  } while (0)
#endif
__attribute__((noreturn)) void lispm_report_error(Sym err);

/* pc must be between M.program and M.program_end page */
Sym lispm_parse(const char *pc);


Sym lispm_st_obj_alloc(unsigned k, Sym *vals);
static inline Sym lispm_cons_alloc(Sym car, Sym cdr) {
  Sym cons[2] = {car, cdr};
  return lispm_st_obj_alloc(LISPM_ST_OBJ_CONS, cons);
}

Sym *lispm_st_obj_unpack(Sym s);
Sym *lispm_cons_unpack_user(Sym a);

Sym lispm_alloc_cons(Sym car, Sym cdr);

Sym lispm_alloc_span(Sym page, Sym offs, Sym len);

Sym lispm_evcap_quote(Sym a, Sym c);
Sym lispm_evquote(Sym a);
void lispm_args_unpack2(Sym a, Sym *f, Sym *s);
