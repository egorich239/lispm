#include "lispm.h"

#define M lispm

/* error reporting */
__attribute__((noreturn)) void lispm_report_error(Sym err) {
  LISPM_ASSERT(M.stack);
  M.stack[0] = err;
  lispm_rt_throw();
}

/* stack functions */
Sym lispm_st_obj_alloc(unsigned k, Sym *vals) {
  const unsigned sz = lispm_st_obj_st_size(k);
  M.sp -= sz;
  LISPM_EVAL_CHECK(M.pp < M.sp, LISPM_ERR_OOM);
  Sym *sp = M.sp;
  sp[0] = vals[0], sp[1] = vals[1];
  if (sz == 3) sp[2] = vals[2];
  return lispm_make_st_obj(k, M.sp - M.stack);
}
Sym *lispm_st_obj_unpack(Sym s) { return M.stack + lispm_st_obj_st_offs(s); }
static Sym gc0(Sym s, unsigned high_mark, unsigned depth) {
  /* TODO: guarantee that gc0 over list (of atoms) is non-recursive */
  static Sym NILS[3] = {};
  if (!lispm_sym_is_st_obj(s) || lispm_st_obj_st_offs(s) >= high_mark) return s;
  unsigned sz = lispm_st_obj_st_size(s);
  Sym res = lispm_st_obj_alloc(lispm_st_obj_kind(s), NILS);
  Sym *t = M.stack + lispm_st_obj_st_offs(res);
  Sym *f = M.stack + lispm_st_obj_st_offs(s) + sz;
  for (Sym *m = t + sz; m != t;)
    *--m = gc0(*--f, high_mark, depth);
  return lispm_st_obj_offset_by(res, depth);
}
static Sym gc(Sym root, unsigned high_mark) {
  unsigned low_mark = M.sp - M.stack;
  root = gc0(root, high_mark, high_mark - low_mark);
  const unsigned lowest_mark = M.sp - M.stack;
  while (lowest_mark < low_mark)
    M.stack[--high_mark] = M.stack[--low_mark];
  return root;
}

/* htable functions */
#define LITERAL_NBUILTIN_BIT LISPM_UPPER_BITS(1)
static inline unsigned hashf(const char *b, const char *e, unsigned seed) {
  unsigned hash = seed;
  while (b != e)
    hash = 33 * hash + ((unsigned char)*b++);
  hash ^= (hash >> M.htable_index_shift);
  hash = (2654435769u * hash) >> M.htable_index_shift;
  LISPM_ASSERT(hash < M.htable_index_size);
  return hash;
}
static inline int str_eq(const char *b, const char *e, const char *h) {
  while (b != e)
    if (*b++ != *h++) return 0;
  return !*h;
}
static inline Sym lispm_literal_is_builtin(Sym s) {
  LISPM_ASSERT(lispm_sym_is_literal(s));
  return !(M.htable[lispm_literal_ht_offs(s)] & LITERAL_NBUILTIN_BIT);
}
static inline Sym lispm_literal_get_assoc(Sym s) {
  LISPM_ASSERT(lispm_sym_is_literal(s));
  const Sym a = M.htable[lispm_literal_ht_offs(s) + 1];
  LISPM_EVAL_CHECK(a != LISPM_SYM_NO_ASSOC, LISPM_ERR_EVAL);
  return a;
}
static inline Sym lispm_literal_set_assoc(Sym s, Sym assoc) {
  LISPM_EVAL_CHECK(!lispm_literal_is_builtin(s), LISPM_ERR_EVAL);
  Sym *a = M.htable + (lispm_literal_ht_offs(s) + 1);
  const Sym old_assoc = *a;
  return *a = assoc, old_assoc;
}
static Sym ensure(const char *b, const char *e) {
  unsigned offset = 5381;
  unsigned *entry;
  for (int attempt = 0; attempt < STRINGS_INDEX_LOOKUP_LIMIT; ++attempt) {
    offset = hashf(b, e, offset);
    entry = M.htable + (2 * offset);
    if (!*entry) goto ensure_insert; /* empty slot */
    if (str_eq(b, e, M.strings + (*entry & ~LITERAL_NBUILTIN_BIT))) return lispm_make_literal(2 * offset); /* found! */
  }
  LISPM_EVAL_CHECK(0, LISPM_ERR_OOM);

ensure_insert:
  LISPM_EVAL_CHECK(M.tp + (e - b + 1) <= M.strings_end, LISPM_ERR_OOM);
  entry[0] = (M.tp - M.strings) | LITERAL_NBUILTIN_BIT;
  entry[1] = LISPM_SYM_NO_ASSOC;
  while (b != e)
    *M.tp++ = *b++;
  *M.tp++ = 0;
  while ((M.tp - M.strings) & 3)
    M.tp++;
  return lispm_make_literal(2 * offset);
}

/* lexer */
#include "lexer.inc.h"

/* Special "symbol" values returned by lexer.
   We utilize the same namespace as builtin functions,
   because lexer never produces special symbols. */
#define TOK_SYM(c) LISPM_MAKE_BUILTIN_FN(((unsigned)(c)))
#define TOK_LPAREN TOK_SYM('(')
#define TOK_RPAREN TOK_SYM(')')
#define TOK_QUOTE  TOK_SYM('\'')

static Sym lex(void) {
  enum { S_ATOM, S_NUM, S_COMMENT, S_INIT } state = S_INIT;
  _Static_assert(LEX_IS_ATOM_SYM(S_ATOM), "S_ATOM must match the category");
  _Static_assert(LEX_IS_DIGIT(S_NUM), "S_NUM must match the category");
  const char *token_begin = M.pc;
  unsigned token_val = 0;
  unsigned c, cat;
  for (; M.pc < M.program_end; ++M.pc) {
    c = (unsigned char)*M.pc;
    if (c >= 128) goto lex_fail;
    cat = LEX_CHAR_CAT(c);
    switch (state) {
    case S_INIT:
      if (c <= ' ') continue;
      if (c == ';') {
        state = S_COMMENT;
        continue;
      }
      if (LEX_IS_TOK(cat)) return ++M.pc, TOK_SYM(c);
      if (LEX_IS_ATOM_SYM(cat)) {
        token_begin = M.pc--, state = cat;
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
  LISPM_EVAL_CHECK(0, LISPM_ERR_LEX);
lex_atom:
  return ensure(token_begin, M.pc);
lex_num:
  if (token_val != 0 && *token_begin == '0') goto lex_fail;
  if (lispm_shortnum_can_represent(token_val)) return lispm_make_shortnum(token_val);
  Sym arr[2] = {lispm_make_shortnum(token_val >> LISPM_SHORTNUM_BITS),
                lispm_make_shortnum(token_val & ((1u << LISPM_SHORTNUM_BITS) - 1))};
  return lispm_st_obj_alloc(LISPM_ST_OBJ_LONGNUM, arr);
}

/* parser */
static Sym LITERAL_QUOTE; /* initialized during builtin initialization */
static Sym lispm_parse0(Sym tok) {
  if (lispm_sym_is_atom(tok)) return tok;
  LISPM_EVAL_CHECK(tok == TOK_LPAREN || tok == TOK_QUOTE, LISPM_ERR_PARSE);
  Sym la = lex();
  if (tok == TOK_QUOTE) return lispm_cons_alloc(LITERAL_QUOTE, lispm_cons_alloc(lispm_parse0(la), LISPM_SYM_NIL));
  if (la == TOK_RPAREN) return LISPM_SYM_NIL;
  unsigned low_mark = M.pp - M.stack;
  while (la != TOK_RPAREN) {
    LISPM_EVAL_CHECK(++M.pp < M.sp, LISPM_ERR_PARSE);
    M.pp[-1] = lispm_parse0(la);
    la = lex();
  }
  Sym res = lispm_cons_alloc(*--M.pp, LISPM_SYM_NIL);
  while (low_mark < (M.pp - M.stack))
    res = lispm_cons_alloc(*--M.pp, res);
  return res;
}
Sym lispm_parse(const char *pc) {
  LISPM_ASSERT(M.program <= pc && pc < M.program_end);
  M.pc = pc;
  return lispm_parse0(lex());
}

/* eval */
static Sym eval0(Sym e);
static Sym evcap0(Sym p, Sym c);

Sym *lispm_cons_unpack_user(Sym a) {
  /* cons unpack on user input; soft failure mode */
  LISPM_EVAL_CHECK(lispm_sym_is_cons(a), LISPM_ERR_EVAL);
  return lispm_st_obj_unpack(a);
}
Sym lispm_evcap_quote(Sym a, Sym c) { return c; }
Sym lispm_evquote(Sym a) {
  Sym *cons = lispm_cons_unpack_user(a);
  LISPM_EVAL_CHECK(lispm_sym_is_nil(cons[1]), LISPM_ERR_EVAL);
  return cons[0];
}
void lispm_args_unpack2(Sym a, Sym *f, Sym *s) {
  Sym *cons = lispm_cons_unpack_user(a);
  *f = cons[0];
  *s = lispm_evquote(cons[1]);
}
static Sym lispm_evcap_con(Sym bs, Sym c) {
  Sym *cons, b;
  while (!lispm_sym_is_nil(bs)) {
    cons = lispm_cons_unpack_user(bs), b = cons[0], bs = cons[1];
    cons = lispm_cons_unpack_user(b);
    c = evcap0(cons[0], c);
    c = evcap0(lispm_evquote(cons[1]), c);
  }
  return c;
}
static Sym lispm_evcon(Sym bs) {
  Sym *cons, b;
  while (!lispm_sym_is_nil(bs)) {
    cons = lispm_cons_unpack_user(bs), b = cons[0], bs = cons[1];
    cons = lispm_cons_unpack_user(b);
    if (!lispm_sym_is_nil(eval0(cons[0]))) return eval0(lispm_evquote(cons[1]));
  }
  LISPM_EVAL_CHECK(0, LISPM_ERR_EVAL);
}
static void lispm_restore_shadow(Sym s, Sym s0) {
  Sym *cons, a;
  while (s != s0) {
    cons = lispm_st_obj_unpack(s), a = cons[0], s = cons[1];
    cons = lispm_st_obj_unpack(a);
    lispm_literal_set_assoc(cons[0], cons[1]);
  }
}
static Sym lispm_evcap_remove_bindings(Sym c, Sym c0) {
  if (c == c0) return c0;
  Sym *cons, a;
  cons = lispm_st_obj_unpack(c), a = cons[0], c = cons[1];
  cons = lispm_st_obj_unpack(a);
  c = lispm_evcap_remove_bindings(c, c0);
  return cons[1] == LISPM_SYM_BINDING ? c : lispm_cons_alloc(a, c);
}
static Sym lispm_evcap_lambda(Sym t, Sym c0) {
  Sym p, b, n, c = c0, *cons;
  lispm_args_unpack2(t, &p, &b);
  while (!lispm_sym_is_nil(p)) {
    cons = lispm_cons_unpack_user(p), n = cons[0], p = cons[1];
    c = lispm_cons_alloc(lispm_cons_alloc(n, lispm_literal_set_assoc(n, LISPM_SYM_BINDING)), c);
  }
  c = evcap0(b, c);
  lispm_restore_shadow(c, c0);
  return lispm_evcap_remove_bindings(c, c0);
}
static Sym lispm_evlambda(Sym t) {
  Sym p, b;
  lispm_args_unpack2(t, &p, &b);
  Sym arr[3] = {lispm_evcap_lambda(t, LISPM_SYM_NIL), p, b};
  return lispm_st_obj_alloc(LISPM_ST_OBJ_LAMBDA, arr);
}
static Sym lispm_evcap_let(Sym t, Sym c0) {
  Sym a, b, e, e1, n, c = c0, *cons;
  cons = lispm_cons_unpack_user(t), a = cons[0], e = cons[1];
  while (!lispm_sym_is_nil(a)) {
    cons = lispm_cons_unpack_user(a), b = cons[0], a = cons[1];
    cons = lispm_cons_unpack_user(b), n = cons[0], e1 = cons[1];
    c = evcap0(lispm_evquote(e1), c);
    c = lispm_cons_alloc(lispm_cons_alloc(n, lispm_literal_set_assoc(n, LISPM_SYM_BINDING)), c);
  }
  c = evcap0(e, c); /* TODO: this suffix is the same for the evcap_lambda! */
  lispm_restore_shadow(c, c0);
  return lispm_evcap_remove_bindings(c, c0);
}
static Sym lispm_evlet(Sym e) {
  Sym c, b, a, n, v, *cons;
  cons = lispm_cons_unpack_user(e), c = cons[0], b = cons[1];
  b = lispm_evquote(b);
  while (!lispm_sym_is_nil(c)) {
    cons = lispm_cons_unpack_user(c), a = cons[0], c = cons[1];
    cons = lispm_cons_unpack_user(a), n = cons[0], v = cons[1];
    lispm_literal_set_assoc(n, eval0(lispm_evquote(v)));
  }
  return eval0(b);
}
static Sym EVAL(Sym e) { /* can be done in lisp, but let's not */ return eval0(lispm_evquote(e)); }
static Sym CONS(Sym a) {
  Sym x, y;
  lispm_args_unpack2(a, &x, &y);
  return lispm_cons_alloc(x, y);
}
static Sym CAR(Sym a) { return lispm_cons_unpack_user(lispm_evquote(a))[0]; }
static Sym CDR(Sym a) { return lispm_cons_unpack_user(lispm_evquote(a))[1]; }
static Sym ATOM(Sym a) { return lispm_sym_is_atom(lispm_evquote(a)) ? LISPM_SYM_T : LISPM_SYM_NIL; }
static Sym EQ(Sym a) {
  Sym x, y;
  lispm_args_unpack2(a, &x, &y);
  if (!lispm_sym_is_atom(x) || !lispm_sym_is_atom(y)) return LISPM_SYM_NIL;
  if (x == y) return LISPM_SYM_T;
  if (!lispm_sym_is_longnum(x) || !lispm_sym_is_longnum(y)) return LISPM_SYM_NIL;
  Sym *cx = lispm_st_obj_unpack(x), *cy = lispm_st_obj_unpack(y);
  return cx[0] == cy[0] && cx[1] == cy[1];
}

static const struct Builtin *builtin(Sym s) {
  LISPM_ASSERT(lispm_sym_is_builtin_fn(s));
  return M.builtins + lispm_builtin_fn_ft_offs(s);
}

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
  Sym a, t, b, *cons;
  cons = lispm_cons_unpack_user(p), a = cons[0], t = cons[1];
  if (lispm_sym_is_literal(a) && lispm_literal_is_builtin(a)) {
    b = lispm_literal_get_assoc(a);
    const struct Builtin *bi = builtin(b); /* special form */
    if (bi->evcap) return (bi->evcap)(t, c);
  }
  while (!lispm_sym_is_nil(p)) {
    cons = lispm_cons_unpack_user(p), a = cons[0], p = cons[1];
    c = evcap0(a, c);
  }
  return c;
}
static Sym reverse_inplace(Sym li) {
  /* not a public interface because it actually changes the value of a symbol, i.e. has side-effects */
  LISPM_ASSERT(lispm_sym_is_nil(li) || lispm_sym_is_cons(li));
  Sym cur = li, prev = LISPM_SYM_NIL, next, *cons;
  while (!lispm_sym_is_nil(cur)) {
    cons = lispm_st_obj_unpack(cur), next = cons[1];
    M.stack[lispm_st_obj_st_offs(cur) + 1] = prev;
    prev = cur, cur = next;
  }
  /* TODO: adapt to non-nil-terminating sequences */
  return prev;
}
static Sym evlis(Sym e) {
  Sym res = LISPM_SYM_NIL, a, *cons;
  while (!lispm_sym_is_nil(e)) {
    cons = lispm_cons_unpack_user(e), a = cons[0], e = cons[1];
    res = lispm_cons_alloc(eval0(a), res);
  }
  return reverse_inplace(res);
}
static Sym evapply(Sym e) {
  Sym f, a, *cons;
  cons = lispm_cons_unpack_user(e), f = cons[0], a = cons[1];
  int eval_args = 0;
  if (lispm_sym_is_cons(f)) { /* rewrite nested calls */
    f = eval0(f);
    eval_args = 1;
  }
  if (lispm_sym_is_literal(f)) f = lispm_literal_get_assoc(f);          /* resolve literal */
  if (!lispm_sym_is_builtin_fn(f) || !builtin(f)->evcap) eval_args = 1; /* not a special form */
  if (eval_args) a = evlis(a);

  if (lispm_sym_is_builtin_fn(f)) return builtin(f)->eval(a);
  LISPM_EVAL_CHECK(lispm_sym_is_lambda(f), LISPM_ERR_EVAL);
  Sym c, p, b, as, n, v, *la;
  la = lispm_st_obj_unpack(f), c = la[0], p = la[1], b = la[2];
  Sym s = LISPM_SYM_NIL;
  while (!lispm_sym_is_nil(c)) { /* captures */
    cons = lispm_st_obj_unpack(c), as = cons[0], c = cons[1];
    cons = lispm_st_obj_unpack(as), n = cons[0], v = cons[1];
    LISPM_ASSERT(v != LISPM_SYM_BINDING);
    s = lispm_cons_alloc(lispm_cons_alloc(n, lispm_literal_set_assoc(n, v)), s);
  }
  while (!lispm_sym_is_nil(p)) { /* params */
    cons = lispm_cons_unpack_user(p), n = cons[0], p = cons[1];
    cons = lispm_cons_unpack_user(a), v = cons[0], a = cons[1];
    s = lispm_cons_alloc(lispm_cons_alloc(n, lispm_literal_set_assoc(n, v)), s);
  }
  LISPM_EVAL_CHECK(lispm_sym_is_nil(a), LISPM_ERR_EVAL);
  Sym res = eval0(b);
  return lispm_restore_shadow(s, LISPM_SYM_NIL), res;
}
static Sym eval0(Sym e) {
  if (lispm_sym_is_shortnum(e)) return e;
  if (lispm_sym_is_literal(e)) return lispm_literal_get_assoc(e);
  unsigned mark = M.sp - M.stack;
  return gc(evapply(e), mark);
}

/* API */
static inline int lispm_sym_is_t(Sym e) { return e == LISPM_SYM_T; }
static inline int lispm_is_valid_result(Sym e) {
  return lispm_sym_is_nil(e) || lispm_sym_is_t(e) || lispm_sym_is_atom(e) || lispm_sym_is_cons(e);
}
static void lispm_main(void) {
  M.htable_index_size = (M.htable_end - M.htable) >> 1;
  M.htable_index_shift = __builtin_clz(M.htable_index_size) + 1;

  int i = 0;
  for (const struct Builtin *bi = M.builtins; bi->name; ++bi, ++i) {
    const char *n = bi->name;
    Sym s = ensure(n, n + __builtin_strlen(n));
    unsigned *entry = M.htable + lispm_literal_ht_offs(s);
    entry[0] &= ~LITERAL_NBUILTIN_BIT;
    entry[1] = bi->eval ? LISPM_MAKE_BUILTIN_FN(i) : LISPM_SYM_T + i;
    if (bi->store) *bi->store = s;
  }
  Sym p = lispm_parse(M.pc);
  M.stack[1] = eval0(p);
  M.stack[0] = lispm_is_valid_result(M.stack[1]) ? LISPM_SYM_NIL : LISPM_ERR_EVAL;
}

Sym lispm_exec(void) {
  LISPM_ASSERT(lispm_is_valid_config());
  lispm_rt_try(lispm_main);
  return M.stack[0] != LISPM_SYM_NIL ? M.stack[0]  /* lex/parse/eval error */
                                     : M.stack[1]; /* regular result */
}

static const struct Builtin LISPM_CORE_BUILTINS[]
    __attribute__((section(".lispm.rodata.builtins.core"), aligned(16), used)) = {
        {"t"},
        {"quote", lispm_evquote, lispm_evcap_quote, &LITERAL_QUOTE},
        {"cond", lispm_evcon, lispm_evcap_con},
        {"lambda", lispm_evlambda, lispm_evcap_lambda},
        {"let", lispm_evlet, lispm_evcap_let},
        {"atom", ATOM},
        {"cons", CONS},
        {"car", CAR},
        {"cdr", CDR},
        {"eq", EQ},
        {"eval", EVAL},
};
