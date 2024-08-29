#include "lispm.h"

#define M           lispm
#define NIL         lispm_sym_is_nil
#define ASSOC_SET   lispm_literal_set_assoc
#define ASSOC_GET   lispm_literal_get_assoc
#define C(car, cdr) lispm_cons_alloc((struct Cons){(car), (cdr)})

typedef struct Cons Cons;

#if LISPM_CONFIG_VERBOSE
#define LISPM_TRACE(event, ...)                                                                                        \
  do {                                                                                                                 \
    if (M.trace.event) M.trace.event(__VA_ARGS__);                                                                     \
  } while (0)
#else
#define LISPM_TRACE(...) ((void)0)
#endif

/* error reporting */
__attribute__((noreturn)) void lispm_report_error(Sym err, Sym ctx) {
  LISPM_ASSERT(M.stack);
  M.stack[0] = err;
  M.stack[1] = ctx;
  lispm_rt_throw();
}

/* stack functions */
Sym lispm_st_obj_alloc(unsigned k, Sym *vals) {
  const unsigned sz = lispm_st_obj_st_size(k);
  M.sp -= sz;
  LISPM_EVAL_CHECK(M.pp < M.sp, LISPM_ERR_OOM, "stack space exhausted; sp:", lispm_make_shortnum(M.sp - M.stack));
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
static inline Sym ASSOC_GET(Sym s) {
  LISPM_ASSERT(lispm_sym_is_literal(s));
  const Sym a = M.htable[lispm_literal_ht_offs(s) + 1];
  LISPM_EVAL_CHECK(a != LISPM_SYM_NO_ASSOC, LISPM_ERR_EVAL, "unbound literal: ", s);
  return a;
}
static inline Sym ASSOC_SET(Sym s, Sym assoc) {
  LISPM_EVAL_CHECK(!lispm_literal_is_builtin(s), LISPM_ERR_EVAL, "cannot bind to builtin: ", s);
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
  LISPM_EVAL_CHECK(0, LISPM_ERR_OOM, "hash table exhausted, last insert position: ", lispm_make_shortnum(offset));

ensure_insert:
  LISPM_EVAL_CHECK(M.tp + (e - b + 1) <= M.strings_end, LISPM_ERR_OOM,
                   "strings table exhausted, tp: ", lispm_make_shortnum(M.tp - M.strings));
  entry[0] = (M.tp - M.strings) | LITERAL_NBUILTIN_BIT;
  entry[1] = LISPM_SYM_NO_ASSOC;
  while (b != e)
    *M.tp++ = *b++;
  *M.tp++ = 0;
  while ((M.tp - M.strings) & 3)
    M.tp++;
  return lispm_make_literal(2 * offset);
}

/* builtins support */
Sym lispm_builtin_as_sym(const struct Builtin *bi) { return LISPM_MAKE_BUILTIN_SYM(bi - lispm.builtins); }

/* lexer */
#include "lexer.inc.h"

/* Lexer produces atoms: literals, short numbers, full numbers.
   Apart from them lexer can also produce individual delimiters.
   Lexer can never produce a builtin symbol, since it is not a lexical category.
   Hence we reuse the namespace of builtins to represent delimiters.

   TODO: allocate proper space for lexemes, for better error diagnostics */
#define TOK_SYM(c) LISPM_MAKE_BUILTIN_SYM(((unsigned)(c)))
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
          !__builtin_uadd_overflow(token_val, (unsigned)(c - '0'), &token_val) &&
          lispm_shortnum_can_represent(token_val))
        continue;
      if (LEX_IS_DELIM(cat)) goto lex_num;
      goto lex_fail;
    }
  }
  if (state == S_ATOM) goto lex_atom;
  if (state == S_NUM) goto lex_num;
lex_fail:
  LISPM_EVAL_CHECK(0, LISPM_ERR_LEX, "lexer error at offset: ", lispm_make_shortnum(M.pc - M.program));
lex_atom:
  return ensure(token_begin, M.pc);
lex_num:
  if (token_val != 0 && *token_begin == '0') goto lex_fail;
  return lispm_make_shortnum(token_val);
}

/* parser */
static Sym LITERAL_QUOTE; /* initialized during builtin initialization */
static Sym lispm_parse0(Sym tok) {
  if (lispm_sym_is_atom(tok)) return tok;
  LISPM_EVAL_CHECK(tok == TOK_LPAREN || tok == TOK_QUOTE, LISPM_ERR_PARSE, "unexpected token: ", tok);
  Sym la = lex();
  if (tok == TOK_QUOTE) return C(LITERAL_QUOTE, C(lispm_parse0(la), LISPM_SYM_NIL));
  if (la == TOK_RPAREN) return LISPM_SYM_NIL;
  unsigned low_mark = M.pp - M.stack;
  while (la != TOK_RPAREN) {
    LISPM_EVAL_CHECK(++M.pp < M.sp, LISPM_ERR_PARSE,
                     "stack exhausted during parsing, pp: ", lispm_make_shortnum(M.pp - M.stack));
    M.pp[-1] = lispm_parse0(la);
    la = lex();
  }
  Sym res = C(*--M.pp, LISPM_SYM_NIL);
  while (low_mark < (M.pp - M.stack))
    res = C(*--M.pp, res);
  return res;
}
Sym lispm_parse(const char *pc, const char *pc_end) {
  LISPM_ASSERT(M.program <= pc && pc <= pc_end && pc_end <= M.program_end);
  const char *old_pc_end = M.program_end;
  M.pc = pc;
  M.program_end = pc_end;
  Sym res = lispm_parse0(lex());
  M.program_end = old_pc_end;
  return res;
}

/* eval */
static Sym eval0(Sym e);
static Sym evcap0(Sym p, Sym c);

Cons lispm_cons_unpack_user(Sym ref) {
  /* cons unpack on user input; soft failure mode */
  LISPM_EVAL_CHECK(lispm_sym_is_cons(ref), LISPM_ERR_EVAL, "cons expected, got: ", ref);
  return lispm_cons_unpack(ref);
}
Sym lispm_evcap_quote(Sym a, Sym c) { return c; }
Sym lispm_evquote(Sym ref) {
  Cons cons = lispm_cons_unpack_user(ref);
  LISPM_EVAL_CHECK(NIL(cons.cdr), LISPM_ERR_EVAL, "single-element list expected, got: ", ref);
  return cons.car;
}
void lispm_args_unpack2(Sym ref, Sym *f, Sym *s) {
  Cons c1 = lispm_cons_unpack_user(ref);
  Cons c2 = lispm_cons_unpack_user(c1.cdr);
  LISPM_EVAL_CHECK(NIL(c2.cdr), LISPM_ERR_EVAL, "two-elements list expected, got: ", ref);
  *f = c1.car;
  *s = c2.car;
}
static Sym lispm_evcap_remove_bindings(Sym c, Sym c0);
static Sym lispm_evcap_con(Sym bs, Sym c) {
  Sym cond, expr;
  while (!NIL(bs)) {
    lispm_args_unpack2(lispm_cons_next_user(&bs), &cond, &expr);
    Sym c0 = c;
    c = evcap0(cond, c0);
    c = lispm_evcap_remove_bindings(c, c0); /* TODO: test this line */
    c = evcap0(expr, c);
  }
  return c;
}
static Sym lispm_evcon(Sym bs) {
  Sym cond, expr;
  while (!NIL(bs)) {
    lispm_args_unpack2(lispm_cons_next_user(&bs), &cond, &expr);
    if (!NIL(eval0(cond))) return eval0(expr);
  }
  LISPM_EVAL_CHECK(0, LISPM_ERR_EVAL, "condtion branches exhausted", LISPM_SYM_NO_ASSOC);
}
static void lispm_restore_shadow(Sym s, Sym s0) {
  Sym *cons, a;
  while (s != s0) {
    Cons as = lispm_cons_unpack(lispm_cons_next(&s));
    ASSOC_SET(as.car, as.cdr);
  }
}
static Sym lispm_evcap_remove_bindings(Sym c, Sym c0) {
  /* TODO: get rid of non-tail recursion */
  if (c == c0) return c0;
  Sym as_ref = lispm_cons_next(&c);
  Cons as = lispm_cons_unpack(as_ref);
  c = lispm_evcap_remove_bindings(c, c0);
  return as.cdr == LISPM_SYM_BINDING ? c : C(as_ref, c);
}
static Sym lispm_evcap_lambda(Sym t, Sym c0) {
  Sym c = c0, p, b;
  lispm_args_unpack2(t, &p, &b);
  while (!NIL(p)) {
    Sym name = lispm_cons_next_user(&p);
    c = C(C(name, ASSOC_SET(name, LISPM_SYM_BINDING)), c);
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
  Sym c = c0, binds, expr;
  lispm_args_unpack2(t, &binds, &expr);
  while (!NIL(binds)) {
    Sym bind = lispm_cons_next_user(&binds), name, expr;
    lispm_args_unpack2(bind, &name, &expr);
    c = evcap0(expr, c);
    c = C(C(name, ASSOC_SET(name, LISPM_SYM_BINDING)), c);
  }
  c = evcap0(expr, c); /* TODO: this suffix is the same for the evcap_lambda! */
  lispm_restore_shadow(c, c0);
  return lispm_evcap_remove_bindings(c, c0);
}
static Sym lispm_evlet(Sym e) {
  Sym s = LISPM_SYM_NIL, binds, expr;
  lispm_args_unpack2(e, &binds, &expr);
  while (!NIL(binds)) {
    Sym bind = lispm_cons_next_user(&binds), name, expr;
    lispm_args_unpack2(bind, &name, &expr);
    s = C(C(name, ASSOC_SET(name, eval0(expr))), s);
  }
  Sym res = eval0(expr);
  return lispm_restore_shadow(s, LISPM_SYM_NIL), res;
}
static Sym LIST(Sym a) { return a; }
static Sym CONS(Sym a) {
  Sym x, y;
  lispm_args_unpack2(a, &x, &y);
  return C(x, y);
}
static Sym CAR(Sym a) { return lispm_cons_unpack_user(lispm_evquote(a)).car; }
static Sym CDR(Sym a) { return lispm_cons_unpack_user(lispm_evquote(a)).cdr; }
static Sym ATOM(Sym a) { return lispm_sym_is_atom(lispm_evquote(a)) ? LISPM_SYM_T : LISPM_SYM_NIL; }
static Sym EQ(Sym a) {
  Sym x, y;
  lispm_args_unpack2(a, &x, &y);
  return lispm_sym_is_atom(x) && x == y ? LISPM_SYM_T : LISPM_SYM_NIL;
}
static Sym PANIC(Sym a) { LISPM_EVAL_CHECK(0, LISPM_ERR_EVAL, "panic: ", a); }

static const struct Builtin *builtin(Sym s) {
  LISPM_ASSERT(lispm_sym_is_builtin_sym(s));
  return M.builtins + lispm_builtin_sym_offs(s);
}

static Sym evcap0(Sym p, Sym c) {
  if (lispm_sym_is_shortnum(p)) return c; /* unsigned: no need to capture */
  if (lispm_sym_is_literal(p)) {
    if (lispm_literal_is_builtin(p)) return c; /* builtins: no need to capture */
    const Sym a = ASSOC_GET(p);
    if (a == LISPM_SYM_CAPTURED || a == LISPM_SYM_BINDING) return c; /* already captured */

    /* not captured yet, mark it as such */
    return C(C(p, ASSOC_SET(p, LISPM_SYM_CAPTURED)), c);
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
  Sym fn, args, b;
  Cons cons = lispm_cons_unpack_user(p);
  fn = cons.car, args = cons.cdr;
  if (lispm_sym_is_literal(fn) && lispm_literal_is_builtin(fn)) {
    const struct Builtin *bi = builtin(ASSOC_GET(fn));
    if (bi->evcap) return (bi->evcap)(args, c); /* special form */
  }
  /* if not a special form, then evaluate captures of all elements */
  while (!NIL(p)) { /* NOTE: p! */
    Sym arg = lispm_cons_next(&p);
    c = evcap0(arg, c); /* BUG: remove bindings before proceeding! */
  }
  return c;
}
static Sym reverse_inplace(Sym li) {
  /* not a public interface because it actually changes the value of a symbol, i.e. has side-effects */
  LISPM_ASSERT(NIL(li) || lispm_sym_is_cons(li));
  Sym prev = LISPM_SYM_NIL, cur = li, next;
  while (!NIL(cur)) {
    Cons cons = lispm_cons_unpack(cur);
    next = cons.cdr;
    M.stack[lispm_st_obj_st_offs(cur) + 1] = prev;
    prev = cur, cur = next;
  }
  /* TODO: adapt to non-nil-terminating sequences */
  return prev;
}
static Sym evlis(Sym e) {
  Sym res = LISPM_SYM_NIL, expr, *cons;
  while (!NIL(e)) {
    expr = lispm_cons_next_user(&e);
    res = C(eval0(expr), res);
  }
  return reverse_inplace(res);
}
static int is_lambda_or_builtin_fn(Sym f, const struct Builtin **bi) {
  if (lispm_sym_is_lambda(f)) return 1;
  if (!lispm_sym_is_builtin_sym(f)) return 0;
  *bi = builtin(f);
  return (*bi)->eval != 0;
}
static Sym evapply(Sym e) {
  Cons cons = lispm_cons_unpack_user(e);
  Sym f = cons.car, fn = f, a = cons.cdr;
  int eval_args = 0;
  if (lispm_sym_is_cons(f)) { /* rewrite nested calls */
    fn = f = eval0(f);
    eval_args = 1;
  }
  /* TODO: the lack of `else` here is peculiar.
     Shall `((quote quote) abc)` really lead to evaluating the `abc` *and* applying `quote` to the result???
     Or maybe it shall just be treated as a list '(quote abc)?
     Or as an error? I think it shall be an error. */
  if (lispm_sym_is_literal(f)) fn = ASSOC_GET(f); /* resolve literal */

  const struct Builtin *bi = 0;
  LISPM_EVAL_CHECK(is_lambda_or_builtin_fn(fn, &bi), LISPM_ERR_EVAL, "a function expected, got: ", f);
  if (!bi || !bi->evcap) eval_args = 1; /* not a special form */
  if (eval_args) a = evlis(a);
  LISPM_TRACE(apply_enter, f, fn, a);

  if (bi) return bi->eval(a);
  LISPM_ASSERT(lispm_sym_is_lambda(fn));
  Sym c, p, b, as, n, v, *la;
  la = lispm_st_obj_unpack(fn), c = la[0], p = la[1], b = la[2];
  Sym s = LISPM_SYM_NIL;
  while (!NIL(c)) { /* captures */
    Cons as = lispm_cons_unpack(lispm_cons_next(&c));
    LISPM_ASSERT(as.cdr != LISPM_SYM_BINDING);
    s = C(C(as.car, ASSOC_SET(as.car, as.cdr)), s);
  }
  while (!NIL(p)) { /* params */
    n = lispm_cons_next_user(&p);
    v = lispm_cons_next_user(&a);
    s = C(C(n, ASSOC_SET(n, v)), s);
  }
  LISPM_EVAL_CHECK(NIL(a), LISPM_ERR_EVAL, "too many arguments for call: ", fn);
  Sym res = eval0(b);
  return lispm_restore_shadow(s, LISPM_SYM_NIL), res;
}
static Sym eval0(Sym e) {
  if (lispm_sym_is_shortnum(e)) return e;
  if (lispm_sym_is_literal(e)) return ASSOC_GET(e);
  unsigned mark = M.sp - M.stack;
  Sym res = gc(evapply(e), mark);
  LISPM_TRACE(apply_leave);
  return res;
}

/* API */
Sym lispm_eval(const char *pc, const char *pc_end) { return eval0(lispm_parse(pc, pc_end)); }
static inline int lispm_sym_is_t(Sym e) { return e == LISPM_SYM_T; }
static inline int lispm_is_valid_result(Sym e) {
  return NIL(e) || lispm_sym_is_t(e) || lispm_sym_is_atom(e) || lispm_sym_is_cons(e);
}
static void lispm_main(void) {
  Sym r = lispm_eval(M.pc, M.program_end);
  LISPM_EVAL_CHECK(lispm_is_valid_result(r), LISPM_ERR_EVAL, "invalid result of evaluation: ", r);
  M.stack[0] = r;
}

void lispm_init(void) {
  LISPM_ASSERT(lispm_is_valid_config());
  M.htable_index_size = (M.htable_end - M.htable) >> 1;
  M.htable_index_shift = __builtin_clz(M.htable_index_size) + 1;

  int i = 0;
  for (const struct Builtin *bi = M.builtins; bi->name; ++bi, ++i) {
    const char *n = bi->name;
    Sym s = ensure(n, n + __builtin_strlen(n));
    unsigned *entry = M.htable + lispm_literal_ht_offs(s);
    entry[0] &= ~LITERAL_NBUILTIN_BIT;
    entry[1] = LISPM_MAKE_BUILTIN_SYM(i);
    if (bi->store) *bi->store = s;
  }
}

Sym lispm_exec(void) {
  LISPM_ASSERT(M.htable_index_size != 0); /* check that the machine has been initialized */
  lispm_rt_try(lispm_main);
  return M.stack[0];
}

static const struct Builtin LISPM_CORE_BUILTINS[]
    __attribute__((section(".lispm.rodata.builtins.core"), aligned(16), used)) = {
        {"t"},
        {"quote", lispm_evquote, lispm_evcap_quote, &LITERAL_QUOTE},
        {"cond", lispm_evcon, lispm_evcap_con},
        {"lambda", lispm_evlambda, lispm_evcap_lambda},
        {"let", lispm_evlet, lispm_evcap_let},
        {"atom?", ATOM},
        {"eq?", EQ},
        {"list", LIST},
        {"cons", CONS},
        {"car", CAR},
        {"cdr", CDR},
        {"panic!", PANIC},
};
