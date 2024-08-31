#include "lispm.h"

#define M lispm
#define C lispm_cons_alloc

/* error reporting */
__attribute__((noreturn)) void lispm_panic(Sym ctx) {
  LISPM_ASSERT(M.stack);
  M.stack[0] = LISPM_SYM_ERR;
  M.stack[1] = ctx;
  lispm_rt_throw();
}

/* stack functions */
Sym lispm_st_obj_alloc(unsigned k, Sym *vals) {
  const unsigned sz = lispm_st_obj_st_size(k);
  M.sp -= sz;
  LISPM_EVAL_CHECK(M.pp < M.sp, LISPM_SYM_NIL, oom_stack);
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
  Sym res     = lispm_st_obj_alloc(lispm_st_obj_kind(s), NILS);
  Sym *t      = M.stack + lispm_st_obj_st_offs(res);
  Sym *f      = M.stack + lispm_st_obj_st_offs(s) + sz;
  for (Sym *m = t + sz; m != t;)
    *--m = gc0(*--f, high_mark, depth);
  return lispm_st_obj_offset_by(res, depth);
}
static Sym gc(Sym root, unsigned high_mark) {
  unsigned low_mark          = M.sp - M.stack;
  root                       = gc0(root, high_mark, high_mark - low_mark);
  const unsigned lowest_mark = M.sp - M.stack;
  while (lowest_mark < low_mark)
    M.stack[--high_mark] = M.stack[--low_mark];
  return root;
}

/* htable functions */
static inline int htable_is_literal(unsigned key) { return !(key & 1); }
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
static inline Sym lispm_literal_is_assignable(Sym s) {
  LISPM_ASSERT(lispm_sym_is_literal(s));
  return (M.htable[lispm_literal_ht_offs(s)] & 3) == 2;
}
static const struct Builtin *builtin(Sym s) {
  LISPM_ASSERT(lispm_sym_is_builtin_sym(s));
  return M.builtins + lispm_builtin_sym_offs(s);
}

static inline Sym lispm_literal_get_assoc(Sym s) {
  LISPM_ASSERT(lispm_sym_is_literal(s));
  const Sym a = M.htable[lispm_literal_ht_offs(s) + 1];
  LISPM_EVAL_CHECK(a != LISPM_SYM_NO_ASSOC, s, unbound_symbol, s);
  return a;
}
static inline Sym lispm_literal_set_assoc(Sym s, Sym assoc) {
  LISPM_EVAL_CHECK(lispm_literal_is_assignable(s), s, illegal_bind, s, assoc);
  Sym *a              = M.htable + (lispm_literal_ht_offs(s) + 1);
  const Sym old_assoc = *a;
  return *a           = assoc, old_assoc;
}
static Sym ensure(const char *b, const char *e) {
  unsigned offset = 5381;
  unsigned *entry;
  for (int attempt = 0; attempt < STRINGS_INDEX_LOOKUP_LIMIT; ++attempt) {
    offset = hashf(b, e, offset);
    entry  = M.htable + (2 * offset);
    if (!*entry) goto ensure_insert; /* empty slot */
    if (!htable_is_literal(*entry)) continue;
    if (str_eq(b, e, M.strings + (*entry >> 2))) return lispm_make_literal(2 * offset); /* found! */
  }
  LISPM_EVAL_CHECK(0, LISPM_SYM_NIL, oom_htable);

ensure_insert:
  LISPM_EVAL_CHECK(M.tp + (e - b + 1) <= M.strings_end, LISPM_SYM_NIL, oom_strings);
  entry[0] = ((M.tp - M.strings) << 2) | 2;
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

#define TOK_SYM(c) LISPM_MAKE_SPECIAL_VALUE(512u + ((unsigned)(c)))
#define TOK_LPAREN TOK_SYM('(')
#define TOK_RPAREN TOK_SYM(')')
#define TOK_QUOTE  TOK_SYM('\'')

static Sym lex(void) {
  enum { S_ATOM, S_NUM, S_COMMENT, S_INIT } state = S_INIT;
  _Static_assert(LEX_IS_ATOM_SYM(S_ATOM), "S_ATOM must match the category");
  _Static_assert(LEX_IS_DIGIT(S_NUM), "S_NUM must match the category");
  const char *token_begin = M.pc;
  unsigned token_val      = 0;
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
  LISPM_EVAL_CHECK(0, LISPM_SYM_NIL, lex_error);
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
  LISPM_EVAL_CHECK(tok == TOK_LPAREN || tok == TOK_QUOTE, LISPM_SYM_NIL, parse_error, tok);
  Sym la = lex();
  if (tok == TOK_QUOTE) return C(LITERAL_QUOTE, C(lispm_parse0(la), LISPM_SYM_NIL));
  if (la == TOK_RPAREN) return LISPM_SYM_NIL;
  unsigned low_mark = M.pp - M.stack;
  while (la != TOK_RPAREN) {
    LISPM_EVAL_CHECK(++M.pp < M.sp, LISPM_SYM_NIL, oom_stack);
    M.pp[-1] = lispm_parse0(la);
    la       = lex();
  }
  Sym res = C(*--M.pp, LISPM_SYM_NIL);
  while (low_mark < (M.pp - M.stack))
    res = C(*--M.pp, res);
  return res;
}
Sym lispm_parse(const char *pc, const char *pc_end) {
  LISPM_ASSERT(M.program <= pc && pc <= pc_end && pc_end <= M.program_end);
  const char *old_pc_end = M.program_end;
  M.pc                   = pc;
  M.program_end          = pc_end;
  Sym res                = lispm_parse0(lex());
  M.program_end          = old_pc_end;
  return res;
}

/* eval */
static Sym eval0(Sym e);
static Sym evcap0(Sym p, Sym c);

Sym *lispm_cons_unpack_user(Sym a) {
  /* cons unpack on user input; soft failure mode */
  LISPM_EVAL_CHECK(lispm_sym_is_cons(a), a, panic, "cons expected, got: ", a);
  return lispm_st_obj_unpack(a);
}
Sym lispm_evcap_quote(Sym syn, Sym c) { return c; }
Sym lispm_evquote(Sym a) {
  Sym *cons = lispm_cons_unpack_user(a);
  LISPM_EVAL_CHECK(lispm_sym_is_nil(cons[1]), a, panic, "single-element list expected, got: ", a);
  return cons[0];
}
void lispm_args_unpack2(Sym a, Sym *f, Sym *s) {
  Sym *cons = lispm_cons_unpack_user(a);
  *f        = cons[0];
  *s        = lispm_evquote(cons[1]);
}
static Sym lispm_evcap_con(Sym brans, Sym c) {
  Sym *cons, bran;
  while (!lispm_sym_is_nil(brans)) {
    cons = lispm_cons_unpack_user(brans), bran = cons[0], brans = cons[1];
    cons = lispm_cons_unpack_user(bran);
    c    = evcap0(lispm_evquote(cons[1]), evcap0(cons[0], c));
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
  LISPM_EVAL_CHECK(0, LISPM_SYM_NIL, panic, "condition branches exhausted", LISPM_SYM_NIL);
}
static void lispm_restore_shadow(Sym frame) {
  Sym *cons, asgn;
  while (!lispm_sym_is_nil(frame)) {
    cons = lispm_st_obj_unpack(frame), asgn = cons[0], frame = cons[1];
    cons = lispm_st_obj_unpack(asgn);
    lispm_literal_set_assoc(cons[0], cons[1]);
  }
}
static Sym lispm_evcap_lambda(Sym t, Sym c) {
  Sym parms, body, parm, shadow = LISPM_SYM_NIL, *cons;
  lispm_args_unpack2(t, &parms, &body);
  while (!lispm_sym_is_nil(parms)) {
    cons = lispm_cons_unpack_user(parms), parm = cons[0], parms = cons[1];
    shadow = C(C(parm, lispm_literal_set_assoc(parm, LISPM_SYM_BOUND)), shadow);
  }
  c = evcap0(body, c);
  lispm_restore_shadow(shadow);
  return c;
}
static Sym lispm_evlambda(Sym t) {
  Sym p, b;
  lispm_args_unpack2(t, &p, &b);
  Sym captures = lispm_evcap_lambda(t, LISPM_SYM_NIL);
  Sym arr[3]   = {captures, p, b};
  Sym lambda   = lispm_st_obj_alloc(LISPM_ST_OBJ_LAMBDA, arr);
  lispm_restore_shadow(captures);
  LISPM_TRACE(lambda_cons, lambda);
  return lambda;
}
static Sym lispm_evcap_let(Sym syn, Sym c0) {
  Sym asgns, asgn, expr, e1, name, c = c0, *cons, shadow = LISPM_SYM_NIL;
  cons = lispm_st_obj_unpack(syn), asgns = cons[0], expr = cons[1];
  while (!lispm_sym_is_nil(asgns)) {
    cons = lispm_cons_unpack_user(asgns), asgn = cons[0], asgns = cons[1];
    cons = lispm_cons_unpack_user(asgn), name = cons[0], e1 = cons[1];
    c      = evcap0(lispm_evquote(e1), c);
    shadow = C(C(name, lispm_literal_set_assoc(name, LISPM_SYM_BOUND)), shadow);
  }
  c = evcap0(expr, c); /* TODO: this suffix is the same for the evcap_lambda! */
  lispm_restore_shadow(shadow);
  return c;
}
static Sym lispm_evlet(Sym e) {
  Sym c, b, a, n, v, s = LISPM_SYM_NIL, *cons;
  cons = lispm_cons_unpack_user(e), c = cons[0], b = cons[1];
  b = lispm_evquote(b);
  while (!lispm_sym_is_nil(c)) {
    cons = lispm_cons_unpack_user(c), a = cons[0], c = cons[1];
    cons = lispm_cons_unpack_user(a), n = cons[0], v = cons[1];
    v = eval0(lispm_evquote(v));
    s = C(C(n, lispm_literal_set_assoc(n, v)), s);
  }
  Sym res = eval0(b);
  lispm_restore_shadow(s);
  return res;
}
static Sym LIST(Sym a) { return a; }
static Sym CONS(Sym a) {
  Sym x, y;
  lispm_args_unpack2(a, &x, &y);
  return C(x, y);
}
static Sym CAR(Sym a) { return lispm_cons_unpack_user(lispm_evquote(a))[0]; }
static Sym CDR(Sym a) { return lispm_cons_unpack_user(lispm_evquote(a))[1]; }
static Sym ATOM(Sym a) { return lispm_sym_is_atom(lispm_evquote(a)) ? LISPM_SYM_T : LISPM_SYM_NIL; }
static Sym EQ(Sym a) {
  Sym x, y;
  lispm_args_unpack2(a, &x, &y);
  return lispm_sym_is_atom(x) && x == y ? LISPM_SYM_T : LISPM_SYM_NIL;
}
static Sym PANIC(Sym a) { LISPM_EVAL_CHECK(0, a, panic, "user panic: ", a); }

static Sym evcap0(Sym syn, Sym caps) {
  if (lispm_sym_is_shortnum(syn)) return caps; /* unsigned: no need to capture */
  if (lispm_sym_is_literal(syn)) {
    if (!lispm_literal_is_assignable(syn)) return caps; /* builtins: no need to capture */
    const Sym a = lispm_literal_get_assoc(syn);
    if (a == LISPM_SYM_FREE || a == LISPM_SYM_BOUND) return caps; /* already captured */
    /* not captured yet => capture */
    return C(C(syn, lispm_literal_set_assoc(syn, LISPM_SYM_FREE)), caps);
  }

  LISPM_ASSERT(lispm_sym_is_cons(syn));
  Sym arg, args, *cons;
  cons = lispm_st_obj_unpack(syn), arg = cons[0], args = cons[1];
  if (lispm_sym_is_literal(arg)) {
    Sym fn = lispm_literal_get_assoc(arg);
    if (lispm_sym_is_builtin_sym(fn)) {
      const struct Builtin *bi = builtin(fn);
      if (bi->evcap) return (bi->evcap)(args, caps); /* special form */
    }
  }
  while (!lispm_sym_is_nil(syn)) {
    cons = lispm_st_obj_unpack(syn), arg = cons[0], syn = cons[1];
    caps = evcap0(arg, caps);
  }
  return caps;
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
    res = C(eval0(a), res);
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
  Sym f, fn, a, *cons;
  cons = lispm_cons_unpack_user(e), f = cons[0], a = cons[1];
  LISPM_ASSERT(lispm_sym_is_atom(f) || lispm_sym_is_cons(f));
  LISPM_EVAL_CHECK(!lispm_sym_is_shortnum(f), f, panic, "a function expected, got: ", f);
  /* We do not use eval0 for literals, because this place is the only one
     where we are deliberately interested to resolve special forms. */
  fn = lispm_sym_is_literal(f) ? lispm_literal_get_assoc(f) : eval0(f);

  const struct Builtin *bi = 0;
  LISPM_EVAL_CHECK(is_lambda_or_builtin_fn(fn, &bi), f, panic, "a function expected, got: ", f);
  if (!bi || !bi->evcap) a = evlis(a); /* not a special form, evaluate arguments */
  LISPM_TRACE(apply_enter, f, fn, a);

  if (bi) return bi->eval(a);
  LISPM_ASSERT(lispm_sym_is_lambda(fn));
  Sym c, p, b, as, n, v, *la;
  la = lispm_st_obj_unpack(fn), c = la[0], p = la[1], b = la[2];
  Sym s = LISPM_SYM_NIL;
  while (!lispm_sym_is_nil(c)) { /* captures */
    cons = lispm_st_obj_unpack(c), as = cons[0], c = cons[1];
    cons = lispm_st_obj_unpack(as), n = cons[0], v = cons[1];
    s = C(C(n, lispm_literal_set_assoc(n, v)), s);
  }
  while (!lispm_sym_is_nil(p)) { /* params */
    cons = lispm_cons_unpack_user(p), n = cons[0], p = cons[1];
    cons = lispm_cons_unpack_user(a), v = cons[0], a = cons[1];
    s = C(C(n, lispm_literal_set_assoc(n, v)), s);
  }
  LISPM_EVAL_CHECK(lispm_sym_is_nil(a), fn, panic, "too many arguments for call: ", fn);
  Sym res = eval0(b);
  lispm_restore_shadow(s);
  return res;
}
static Sym eval0(Sym syn) {
  if (lispm_sym_is_shortnum(syn)) return syn;
  if (lispm_sym_is_literal(syn)) {
    Sym value = lispm_literal_get_assoc(syn);
    LISPM_EVAL_CHECK(!lispm_sym_is_builtin_sym(value) || !builtin(value)->evcap, syn, panic,
                     "special form cannot be used as a value, got: ", syn);
    return value;
  }
  unsigned mark = M.sp - M.stack;
  Sym res       = gc(evapply(syn), mark);
  LISPM_TRACE(apply_leave);
  return res;
}

/* API */
Sym lispm_eval(const char *pc, const char *pc_end) { return eval0(lispm_parse(pc, pc_end)); }
static inline int lispm_sym_is_t(Sym e) { return e == LISPM_SYM_T; }
static inline int lispm_is_valid_result(Sym e) {
  return lispm_sym_is_nil(e) || lispm_sym_is_t(e) || lispm_sym_is_atom(e) || lispm_sym_is_cons(e);
}
static void lispm_main(void) {
  Sym r = lispm_eval(M.pc, M.program_end);
  LISPM_EVAL_CHECK(lispm_is_valid_result(r), r, panic, "invalid result of evaluation: ", r);
  M.stack[0] = r;
}

void lispm_init(void) {
  LISPM_ASSERT(lispm_is_valid_config());
  M.tp = M.strings + 4; /* we use zero as sentinel value for missing entry in htable,
                           hence no value can have a zero offset into the strings table */
  M.htable_index_size  = (M.htable_end - M.htable) >> 1;
  M.htable_index_shift = __builtin_clz(M.htable_index_size) + 1;

  int i = 0;
  for (const struct Builtin *bi = M.builtins; bi->name; ++bi, ++i) {
    const char *n   = bi->name;
    Sym s           = ensure(n, n + __builtin_strlen(n));
    unsigned *entry = M.htable + lispm_literal_ht_offs(s);
    entry[0] &= ~2u;
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
        {"err!"},
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
