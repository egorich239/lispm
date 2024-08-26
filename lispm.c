#include "lispm.h"
#include "rt.h"
#include "sym.h"

/* state */
static struct PageDesc *PAGE_TABLE;

/* program text counter */
static const char *PROGRAM;
static const char *PROGRAM_END;
static const char *PC;

/* the hash table */
static unsigned *INDEX;
static char *STRINGS;
static unsigned INDEX_SIZE;
static int HASH_SHIFT_AMOUNT;
static unsigned STRINGS_SIZE;
static unsigned TP;

/* the evaluation stack */
static Sym *STACK;
static Sym *STACK_END;
static unsigned SP; /* grows down */
static unsigned PP; /* parser pointer, grows up */

/* error reporting */
__attribute__((noreturn)) void lispm_report_error(Sym err) {
  ASSERT(STACK);
  STACK[0] = err;
  THROW(1);
}
void lispm_error_message_set(const char *msg) {
  int i = 0;
  while (i + 1 < ERROR_MESSAGE_SIZE && (STRINGS[i++] = *msg++)) {}
}

/* stack functions */
static Sym lispm_st_obj_alloc(unsigned k, Sym **sp) {
  SP -= lispm_st_obj_st_size(k);
  EVAL_CHECK(PP < SP, LISPM_ERR_OOM);
  *sp = STACK + SP;
  return lispm_make_st_obj(k, SP);
}
void lispm_st_obj_unpack2(Sym s, Sym *a, Sym *b) {
  ASSERT(lispm_st_obj_st_size(s) == 2);
  const unsigned base = lispm_st_obj_st_offs(s);
  *a = STACK[base + 0];
  *b = STACK[base + 1];
}
void lispm_st_obj_unpack3(Sym s, Sym *a, Sym *b, Sym *c) {
  ASSERT(lispm_st_obj_st_size(s) == 3);
  const unsigned base = lispm_st_obj_st_offs(s);
  *a = STACK[base + 0];
  *b = STACK[base + 1];
  *c = STACK[base + 2];
}
Sym lispm_st_obj_alloc2(unsigned k, Sym a, Sym b) {
  ASSERT(lispm_st_obj_st_size(k) == 2);
  Sym *p;
  Sym res = lispm_st_obj_alloc(k, &p);
  return p[0] = a, p[1] = b, res;
}
Sym lispm_st_obj_alloc3(unsigned k, Sym a, Sym b, Sym c) {
  ASSERT(lispm_st_obj_st_size(k) == 3);
  Sym *p;
  Sym res = lispm_st_obj_alloc(k, &p);
  return p[0] = a, p[1] = b, p[2] = c, res;
}
static inline Sym lispm_cons_alloc(Sym car, Sym cdr) { return lispm_st_obj_alloc2(LISPM_ST_OBJ_CONS, car, cdr); }
static Sym gc0(Sym s, unsigned high_mark, unsigned depth) {
  if (!lispm_sym_is_st_obj(s) || lispm_st_obj_st_offs(s) >= high_mark) return s;
  unsigned sz = lispm_st_obj_st_size(s);
  Sym *t, res = lispm_st_obj_offset_by(lispm_st_obj_alloc(lispm_st_obj_kind(s), &t), depth);
  Sym *f = STACK + lispm_st_obj_st_offs(s) + sz;
  for (Sym *m = t + sz; m != t;)
    *--m = gc0(*--f, high_mark, depth);
  return res;
}
static Sym gc(Sym root, unsigned high_mark) {
  unsigned low_mark = SP;
  root = gc0(root, high_mark, high_mark - low_mark);
  const unsigned lowest_mark = SP;
  while (lowest_mark < low_mark)
    STACK[--high_mark] = STACK[--low_mark];
  return root;
}

/* htable functions */
#define LITERAL_NBUILTIN_BIT UPPER_BITS(1)
static inline unsigned hashf(const char *b, const char *e, unsigned seed) {
  unsigned hash = seed;
  while (b != e)
    hash = 33 * hash + ((unsigned char)*b++);
  hash ^= (hash >> HASH_SHIFT_AMOUNT);
  return (2654435769u * hash) >> HASH_SHIFT_AMOUNT;
}
static inline int str_eq(const char *b, const char *e, const char *h) {
  while (b != e)
    if (*b++ != *h++) return 0;
  return !*h;
}
static inline Sym lispm_literal_is_builtin(Sym s) {
  ASSERT(lispm_sym_is_literal(s));
  return !(INDEX[lispm_literal_ht_offs(s)] & LITERAL_NBUILTIN_BIT);
}
static inline Sym lispm_literal_get_assoc(Sym s) {
  ASSERT(lispm_sym_is_literal(s));
  const Sym a = INDEX[lispm_literal_ht_offs(s) + 1];
  EVAL_CHECK(a != LISPM_SYM_NO_ASSOC, LISPM_ERR_EVAL);
  return a;
}
static inline Sym lispm_literal_set_assoc(Sym s, Sym assoc) {
  EVAL_CHECK(!lispm_literal_is_builtin(s), LISPM_ERR_EVAL);
  Sym *a = INDEX + (lispm_literal_ht_offs(s) + 1);
  const Sym old_assoc = *a;
  return *a = assoc, old_assoc;
}
Sym lispm_literal_name_span(Sym s) {
  ASSERT(lispm_sym_is_literal(s));
  unsigned offs = INDEX[lispm_literal_ht_offs(s)] & ~LITERAL_NBUILTIN_BIT;
  unsigned len = __builtin_strlen(STRINGS + offs);
  return lispm_st_obj_alloc3(LISPM_ST_OBJ_SPAN, LISPM_PAGE_STRINGS, lispm_make_shortnum(offs),
                             lispm_make_shortnum(len));
}

static Sym ensure(const char *b, const char *e) {
  unsigned offset = 5381;
  int attempt = 0;
  for (; attempt < STRINGS_INDEX_LOOKUP_LIMIT; ++attempt) {
    offset = hashf(b, e, offset);
    ASSERT(offset < INDEX_SIZE);
    unsigned *entry = INDEX + (2 * offset);
    if (!*entry) break; /* empty slot */
    if (str_eq(b, e, STRINGS + (*entry & ~LITERAL_NBUILTIN_BIT))) return lispm_make_literal(2 * offset); /* found! */
  }
  EVAL_CHECK(attempt < STRINGS_INDEX_LOOKUP_LIMIT, LISPM_ERR_OOM);
  EVAL_CHECK(TP + (e - b + 1) <= STRINGS_SIZE, LISPM_ERR_OOM);

  unsigned *entry = INDEX + (2 * offset);
  entry[0] = TP | LITERAL_NBUILTIN_BIT;
  entry[1] = LISPM_SYM_NO_ASSOC;
  while (b != e)
    STRINGS[TP++] = *b++;
  STRINGS[TP++] = 0;
  TP += 3u, TP &= ~3u;
  return lispm_make_literal(2 * offset);
}

/* pages */
inline struct PageDesc *lispm_page_desc(Sym pg) {
  ASSERT(lispm_sym_is_page(pg));
  return PAGE_TABLE + lispm_page_pt_offs(pg);
}
unsigned lispm_page_size(Sym pg, int elt_size_log2) {
  ASSERT(lispm_sym_is_page(pg));
  const struct PageDesc *p = lispm_page_desc(pg);
  return (p->end - p->begin) >> elt_size_log2;
}
void *lispm_page_loc(Sym pg, unsigned offs, unsigned elt_size) {
  ASSERT(lispm_sym_is_page(pg) && !(elt_size & (elt_size - 1)));
  const struct PageDesc *p = PAGE_TABLE + lispm_page_pt_offs(pg);
  ASSERT(offs * elt_size < p->end - p->begin);
  return p->begin + offs * elt_size;
}

/* lexer */
#include "lexer.inc.h"

/* Special "symbol" values returned by lexer.
   We utilize the same namespace as builtin functions,
   because lexer never produces special symbols. */
#define TOK_SYM(c) LISPM_MAKE_BUILTIN_FN(((unsigned)(c)))
#define TOK_LPAREN TOK_SYM('(')
#define TOK_RPAREN TOK_SYM(')')

static Sym lex(void) {
  enum { S_ATOM, S_NUM, S_COMMENT, S_INIT } state = S_INIT;
  _Static_assert(LEX_IS_ATOM_SYM(S_ATOM), "S_ATOM must match the category");
  _Static_assert(LEX_IS_DIGIT(S_NUM), "S_NUM must match the category");
  const char *token_begin = PC;
  unsigned token_val = 0;
  unsigned c, cat;
  for (; PC < PROGRAM_END; ++PC) {
    c = (unsigned char)*PC;
    if (c >= 128) goto lex_fail;
    cat = LEX_CHAR_CAT(c);
    switch (state) {
    case S_INIT:
      if (c <= ' ') continue;
      if (c == ';') {
        state = S_COMMENT;
        continue;
      }
      if (LEX_IS_TOK(cat)) return ++PC, TOK_SYM(c);
      if (LEX_IS_ATOM_SYM(cat)) {
        token_begin = PC--, state = cat;
        continue;
      }
      goto lex_fail;
    case S_COMMENT:
      if (c == '\n') state = S_INIT;
      break;
    case S_ATOM:
      if (LEX_IS_ATOM_SYM(cat)) continue;
      if (LEX_IS_DELIM(cat)) goto lex_atom;
      goto lex_fail;
    case S_NUM:
      if (LEX_IS_DIGIT(cat) && !__builtin_umul_overflow(token_val, 10u, &token_val) &&
          !__builtin_uadd_overflow(token_val, (unsigned)(c - '0'), &token_val))
        continue;
      if (LEX_IS_DELIM(cat)) goto lex_num;
      goto lex_fail;
    }
  }
  if (state == S_ATOM) goto lex_atom;
  if (state == S_NUM) goto lex_num;
lex_fail:
  EVAL_CHECK(0, LISPM_ERR_LEX);
lex_atom:
  return ensure(token_begin, PC);
lex_num:
  if (token_val != 0 && *token_begin == '0') goto lex_fail;
  if (lispm_shortnum_can_represent(token_val)) return lispm_make_shortnum(token_val);
  return lispm_st_obj_alloc2(LISPM_ST_OBJ_LONGNUM, lispm_make_shortnum(token_val >> LISPM_SHORTNUM_BITS),
                             lispm_make_shortnum(token_val & ((1u << LISPM_SHORTNUM_BITS) - 1)));
}

/* parser */
static Sym lispm_parse0(Sym tok) {
  if (lispm_sym_is_atom(tok)) return tok;
  EVAL_CHECK(tok == TOK_LPAREN, LISPM_ERR_PARSE);
  if ((tok = lex()) == TOK_RPAREN) return LISPM_SYM_NIL;
  unsigned low_mark = PP;
  while (tok != TOK_RPAREN) {
    EVAL_CHECK(++PP < SP, LISPM_ERR_PARSE);
    STACK[PP - 1] = lispm_parse0(tok);
    tok = lex();
  }
  Sym res = lispm_cons_alloc(STACK[--PP], LISPM_SYM_NIL);
  while (low_mark < PP)
    res = lispm_cons_alloc(STACK[--PP], res);
  return res;
}
Sym lispm_parse(const char *pc) {
  ASSERT(PROGRAM <= pc && pc < PROGRAM_END);
  PC = pc;
  return lispm_parse0(lex());
}

/* eval */
static Sym eval0(Sym e);
static Sym evcap0(Sym p, Sym c);

void lispm_cons_unpack_user(Sym a, Sym *car, Sym *cdr) {
  /* cons unpack on user input; soft failure mode */
  EVAL_CHECK(lispm_sym_is_cons(a), LISPM_ERR_EVAL);
  lispm_st_obj_unpack2(a, car, cdr);
}
Sym lispm_evcap_quote(Sym a, Sym c) { return c; }
inline Sym lispm_evquote(Sym a) {
  Sym r, n;
  lispm_cons_unpack_user(a, &r, &n);
  EVAL_CHECK(lispm_sym_is_nil(n), LISPM_ERR_EVAL);
  return r;
}
void lispm_args_unpack2(Sym a, Sym *f, Sym *s) {
  lispm_cons_unpack_user(a, f, s);
  *s = lispm_evquote(*s);
}
static Sym lispm_evcap_con(Sym bs, Sym c) {
  Sym b, q, e;
  while (!lispm_sym_is_nil(bs)) {
    lispm_cons_unpack_user(bs, &b, &bs);
    lispm_cons_unpack_user(b, &q, &e);
    c = evcap0(q, c);
    c = evcap0(lispm_evquote(e), c);
  }
  return c;
}
static Sym lispm_evcon(Sym bs) {
  Sym b, c, a;
  while (!lispm_sym_is_nil(bs)) {
    lispm_cons_unpack_user(bs, &b, &bs);
    lispm_cons_unpack_user(b, &c, &a);
    if (!lispm_sym_is_nil(eval0(c))) return eval0(lispm_evquote(a));
  }
  EVAL_CHECK(0, LISPM_ERR_EVAL);
}
static void lispm_restore_shadow(Sym s, Sym s0) {
  while (s != s0) {
    Sym a, n, v;
    lispm_st_obj_unpack2(s, &a, &s);
    lispm_st_obj_unpack2(a, &n, &v);
    lispm_literal_set_assoc(n, v);
  }
}
static Sym lispm_evcap_remove_bindings(Sym c, Sym c0) {
  if (c == c0) return c0;
  Sym a, n, v;
  lispm_st_obj_unpack2(c, &a, &c);
  lispm_st_obj_unpack2(a, &n, &v);
  c = lispm_evcap_remove_bindings(c, c0);
  return v == LISPM_SYM_BINDING ? c : lispm_cons_alloc(a, c);
}
static Sym lispm_evcap_lambda(Sym t, Sym c0) {
  Sym p, b, n, c = c0;
  lispm_args_unpack2(t, &p, &b);
  while (!lispm_sym_is_nil(p)) {
    lispm_cons_unpack_user(p, &n, &p);
    c = lispm_cons_alloc(lispm_cons_alloc(n, lispm_literal_set_assoc(n, LISPM_SYM_BINDING)), c);
  }
  c = evcap0(b, c);
  lispm_restore_shadow(c, c0);
  return lispm_evcap_remove_bindings(c, c0);
}
static Sym lispm_evlambda(Sym t) {
  Sym p, b;
  lispm_args_unpack2(t, &p, &b);
  return lispm_st_obj_alloc3(LISPM_ST_OBJ_LAMBDA, lispm_evcap_lambda(t, LISPM_SYM_NIL), p, b);
}
static Sym lispm_evcap_let(Sym t, Sym c0) {
  Sym a, b, e, e1, n, c = c0;
  lispm_cons_unpack_user(t, &a, &e);
  while (!lispm_sym_is_nil(a)) {
    lispm_cons_unpack_user(a, &b, &a);
    lispm_cons_unpack_user(b, &n, &e1);
    c = evcap0(lispm_evquote(e1), c);
    c = lispm_cons_alloc(lispm_cons_alloc(n, lispm_literal_set_assoc(n, LISPM_SYM_BINDING)), c);
  }
  c = evcap0(e, c); /* TODO: this suffix is the same for the evcap_lambda! */
  lispm_restore_shadow(c, c0);
  return lispm_evcap_remove_bindings(c, c0);
}
static Sym lispm_evlet(Sym e) {
  Sym c, b, a, n, v;
  lispm_cons_unpack_user(e, &c, &b);
  b = lispm_evquote(b);
  while (!lispm_sym_is_nil(c)) {
    lispm_cons_unpack_user(c, &a, &c);
    lispm_cons_unpack_user(a, &n, &v);
    lispm_literal_set_assoc(n, eval0(lispm_evquote(v)));
  }
  return eval0(b);
}
static Sym EVAL(Sym e) { /* can be done in lisp, but let's not */ return eval0(lispm_evquote(e)); }
static struct Builtin BUILTINS[BUILTINS_TABLE_SIZE] = {
    {"T"},
    {"QUOTE", lispm_evquote, lispm_evcap_quote},
    {"COND", lispm_evcon, lispm_evcap_con},
    {"LAMBDA", lispm_evlambda, lispm_evcap_lambda},
    {"LET", lispm_evlet, lispm_evcap_let},
    {"EVAL", EVAL},
    {},
};
static unsigned BUILTINS_SIZE;

static Sym evcap0(Sym p, Sym c) {
  if (lispm_sym_is_shortnum(p)) return c; /* unsigned: no need to capture */
  if (lispm_sym_is_literal(p)) {
    if (lispm_literal_is_builtin(p)) return c; /* builtins: no need to capture */
    const Sym a = lispm_literal_get_assoc(p);
    if (a == LISPM_SYM_CAPTURED || a == LISPM_SYM_BINDING) return c; /* already captured */

    /* not captured yet, mark it as such */
    return lispm_cons_alloc(lispm_cons_alloc(p, lispm_literal_set_assoc(p, LISPM_SYM_CAPTURED)), c);
  }

  /* evcap0 operates on the syntactic structure of the expression.
   * This implies that it either observes a (LIT ...) or ((...) ...) at this
   * point. Unless we observe the former with `LIT` special form, we treat this
   * expression as an application, and evaluate the arguments.
   *
   * This means that:
   * `(LET ((AB (QUOTE T))) (QUOTE AB))` evaluates to `AB`
   * `(LET ((AB (QUOTE T))) ((QUOTE QUOTE) AB))` evaluates to `T`
   */
  Sym a, t, b;
  lispm_cons_unpack_user(p, &a, &t);
  if (lispm_sym_is_literal(a) && lispm_literal_is_builtin(a)) {
    b = lispm_literal_get_assoc(a);
    if (lispm_sym_is_special_form(b)) return BUILTINS[lispm_builtin_fn_ft_offs(b)].evcap(t, c);
  }
  while (!lispm_sym_is_nil(p)) {
    lispm_cons_unpack_user(p, &a, &p);
    c = evcap0(a, c);
  }
  return c;
}
static Sym evlis(Sym e) {
  if (lispm_sym_is_nil(e)) return e;
  Sym a, d;
  lispm_cons_unpack_user(e, &a, &d);
  return lispm_cons_alloc(eval0(a), evlis(d));
}
static Sym evapply(Sym e) {
  Sym f, a;
  lispm_cons_unpack_user(e, &f, &a);
  int eval_args = 0;
  if (lispm_sym_is_cons(f)) { /* rewrite nested calls */
    f = eval0(f);
    eval_args = 1;
  }
  if (lispm_sym_is_literal(f)) f = lispm_literal_get_assoc(f); /* resolve literal */
  if (!lispm_sym_is_special_form(f)) eval_args = 1;
  if (eval_args) a = evlis(a);

  if (lispm_sym_is_builtin_fn(f)) return BUILTINS[lispm_builtin_fn_ft_offs(f)].eval(a);
  EVAL_CHECK(lispm_sym_is_lambda(f), LISPM_ERR_EVAL);
  Sym c, p, b, as, n, v;
  lispm_st_obj_unpack3(f, &c, &p, &b);
  Sym s = LISPM_SYM_NIL;
  while (!lispm_sym_is_nil(c)) { /* captures */
    lispm_st_obj_unpack2(c, &as, &c);
    lispm_st_obj_unpack2(as, &n, &v);
    ASSERT(v != LISPM_SYM_BINDING);
    s = lispm_cons_alloc(lispm_cons_alloc(n, lispm_literal_set_assoc(n, v)), s);
  }
  while (!lispm_sym_is_nil(p)) { /* params */
    lispm_cons_unpack_user(p, &n, &p);
    lispm_cons_unpack_user(a, &v, &a);
    s = lispm_cons_alloc(lispm_cons_alloc(n, lispm_literal_set_assoc(n, v)), s);
  }
  EVAL_CHECK(lispm_sym_is_nil(a), LISPM_ERR_EVAL);
  Sym res = eval0(b);
  return lispm_restore_shadow(s, LISPM_SYM_NIL), res;
}
static Sym eval0(Sym e) {
  if (lispm_sym_is_shortnum(e)) return e;
  if (lispm_sym_is_literal(e)) return lispm_literal_get_assoc(e);
  unsigned mark = SP;
  return gc(evapply(e), mark);
}

/* API */
static inline int lispm_sym_is_t(Sym e) { return e == LISPM_SYM_T; }
static inline int lispm_is_valid_result(Sym e) {
  return lispm_sym_is_nil(e) || lispm_sym_is_t(e) || lispm_sym_is_atom(e) || lispm_sym_is_cons(e);
}
static int lispm_main(struct PageDesc *table, unsigned offs, const struct Builtin *rt) {
  if (!(PAGE_TABLE = table)) return 1;
  for (int i = 0; i < LISPM_PAGE_TABLE_PRELUDE_SIZE; ++i) {
    if (!PAGE_TABLE[i].begin) return 1;
    PAGE_TABLE[i].flags = PAGE_FLAG_RO;
  }

  PROGRAM = table[lispm_page_pt_offs(LISPM_PAGE_PROGRAM)].begin;
  PROGRAM_END = table[lispm_page_pt_offs(LISPM_PAGE_PROGRAM)].end;

  STACK = PAGE_TABLE[lispm_page_pt_offs(LISPM_PAGE_STACK)].begin;
  STACK_END = PAGE_TABLE[lispm_page_pt_offs(LISPM_PAGE_STACK)].end;
  PP = 64;
  SP = STACK_END - STACK;
  if (SP <= PP) return 1;

  STRINGS = PAGE_TABLE[lispm_page_pt_offs(LISPM_PAGE_STRINGS)].begin;
  STRINGS_SIZE = ((char *)PAGE_TABLE[lispm_page_pt_offs(LISPM_PAGE_STRINGS)].end) - STRINGS;
  INDEX = PAGE_TABLE[lispm_page_pt_offs(LISPM_PAGE_INDEX)].begin;
  INDEX_SIZE = (((unsigned *)PAGE_TABLE[lispm_page_pt_offs(LISPM_PAGE_INDEX)].end) - INDEX) / 2;
  if (INDEX_SIZE < 2 || (INDEX_SIZE & (INDEX_SIZE - 1))) return 1;
  HASH_SHIFT_AMOUNT = __builtin_clz(INDEX_SIZE) + 1;

  TP = ERROR_MESSAGE_SIZE;
  while (BUILTINS[BUILTINS_SIZE].name)
    ++BUILTINS_SIZE;
  while (rt && rt->name) {
    if (BUILTINS_SIZE == BUILTINS_TABLE_SIZE) return 1;
    BUILTINS[BUILTINS_SIZE++] = *rt++;
  }
  for (int i = 0; i < BUILTINS_SIZE; ++i) {
    const struct Builtin *bi = BUILTINS + i;
    const char *n = bi->name;
    Sym s = ensure(n, n + __builtin_strlen(n));
    unsigned *entry = INDEX + lispm_literal_ht_offs(s);
    entry[0] &= ~LITERAL_NBUILTIN_BIT;
    entry[1] =
        bi->eval ? (LISPM_MAKE_BUILTIN_FN(i) | (BUILTINS[i].evcap ? LISPM_SPECIAL_FORM_BIT : 0)) : LISPM_SYM_T + i;
  }
  Sym p = lispm_parse(PROGRAM + offs);
  STACK[1] = eval0(p);
  STACK[0] = lispm_is_valid_result(STACK[1]) ? LISPM_SYM_NIL : LISPM_ERR_EVAL;
  return 0;
}

Sym lispm_exec(struct PageDesc *table, unsigned offs, const struct Builtin *rt) {
  int failed_init = 0;
  TRY(failed_init = lispm_main(table, offs, rt));
  return failed_init                 ? LISPM_ERR_INIT /* init error */
         : STACK[0] != LISPM_SYM_NIL ? STACK[0]       /* lex/parse/eval error */
                                     : STACK[1];      /* regular result */
}
