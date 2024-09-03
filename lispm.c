#include "lispm.h"

#define M lispm
#define C lispm_cons_alloc

/* error reporting */
static Sym SYM_ERR;
__attribute__((noreturn)) void lispm_panic(Sym ctx) {
  LISPM_ASSERT(M.stack);
  M.stack[0] = SYM_ERR;
  M.stack[1] = ctx;
  lispm_rt_throw();
}

/* list functions */
unsigned lispm_list_scan(Sym *out, Sym li, unsigned limit) {
  Sym it = li, *cons, val;
  unsigned cntr = 0;
  while (lispm_sym_is_cons(it) && cntr < limit) {
    cons = lispm_st_obj_unpack(it), val = cons[0], it = cons[1];
    out[cntr++] = val;
  }
  return lispm_sym_is_nil(it) ? cntr : ~0u;
}

static Sym list_reverse_inplace(Sym li) {
  /* not a public interface because it actually changes the object state */
  LISPM_ASSERT(lispm_sym_is_nil(li) || lispm_sym_is_cons(li));
  Sym cur = li, prev = LISPM_SYM_NIL, next, *cons;
  while (!lispm_sym_is_nil(cur)) {
    cons = lispm_st_obj_unpack(cur), next = cons[1];
    cons[1] = prev, prev = cur, cur = next;
  }
  return prev;
}

/* stack functions */
Sym lispm_st_obj_alloc(unsigned k) {
  M.sp -= lispm_st_obj_st_size(k);
  LISPM_EVAL_CHECK(M.stack + LISPM_STACK_BOTTOM_OFFSET < M.sp, LISPM_SYM_NIL, oom_stack);
  return lispm_make_st_obj(k, M.sp - M.stack);
}
Sym *lispm_st_obj_unpack(Sym s) { return M.stack + lispm_st_obj_st_offs(s); }
static Sym gc0(Sym s, unsigned high_mark, unsigned depth) {
  /* TODO: guarantee that gc0 over list (of atoms) is non-recursive */
  if (!lispm_sym_is_st_obj(s) || lispm_st_obj_st_offs(s) >= high_mark) return s;
  unsigned sz = lispm_st_obj_st_size(s);
  Sym res = lispm_st_obj_alloc(lispm_st_obj_kind(s));
  Sym *t = M.sp, *f = lispm_st_obj_unpack(s) + sz;
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
#define PARSE_SYM_BOUND(depth) (((depth) << 4) | 7u)
#define PARSE_SYM_FREE(depth)  (((depth) << 4) | 11u)
enum { PARSE_SYM_UNBOUND = PARSE_SYM_BOUND(0) };

static inline unsigned htable_entry_key(Sym s) { return M.htable[lispm_literal_ht_offs(s)]; }
static inline int htable_entry_key_payload(unsigned key) { return key >> 2; }
static inline int htable_entry_is_assignable(Sym s) {
  LISPM_ASSERT(lispm_sym_is_literal(s));
  return (htable_entry_key(s) & 3) == 2;
}
static inline int htable_entry_key_streq(unsigned key, const char *b, const char *e) {
  const char *h = M.strings + htable_entry_key_payload(key);
  while (b != e)
    if (*b++ != *h++) return 0;
  return !*h;
}
static inline Sym htable_entry_get_assoc(Sym s) {
  LISPM_ASSERT(lispm_sym_is_literal(s));
  return M.htable[lispm_literal_ht_offs(s) + 1];
}
static Sym htable_entry_set_assoc(Sym s, Sym assoc) {
  LISPM_ASSERT(lispm_sym_is_literal(s) && htable_entry_is_assignable(s));
  Sym *a = M.htable + (lispm_literal_ht_offs(s) + 1), old_assoc = *a;
  return *a = assoc, old_assoc;
}
static Sym htable_shadow_append(Sym shadow, Sym lit, Sym assoc) {
  return C(C(lit, htable_entry_set_assoc(lit, assoc)), shadow);
}
static void htable_shadow_rollback(Sym shadow) {
  Sym asgn, *cons;
  while (!lispm_sym_is_nil(shadow)) {
    cons = lispm_st_obj_unpack(shadow), asgn = cons[0], shadow = cons[1];
    cons = lispm_st_obj_unpack(asgn);
    M.htable[lispm_literal_ht_offs(cons[0]) + 1] = cons[1];
  }
}
static inline unsigned htable_hashf(const char *b, const char *e, unsigned seed) {
  unsigned hash = seed;
  while (b != e)
    hash = 33 * hash + ((unsigned char)*b++);
  hash ^= (hash >> M.htable_index_shift);
  hash = (2654435769u * hash) >> M.htable_index_shift;
  LISPM_ASSERT(hash < M.htable_index_size);
  return hash;
}
static Sym htable_ensure(const char *b, const char *e, int is_lex) {
  unsigned offset = 5381, *entry;
  Sym lit;
  for (int attempt = 0; attempt < LISPM_STRINGS_INDEX_LOOKUP_LIMIT; ++attempt) {
    offset = htable_hashf(b, e, offset);
    entry = M.htable + (2 * offset);
    lit = lispm_make_literal(2 * offset);
    if (!*entry) goto ensure_insert;                      /* empty slot */
    if (htable_entry_key_streq(*entry, b, e)) return lit; /* found! */
  }
  LISPM_EVAL_CHECK(0, LISPM_SYM_NIL, oom_htable);

ensure_insert:
  LISPM_EVAL_CHECK(M.tp + (e - b + 1) <= M.strings_end, LISPM_SYM_NIL, oom_strings);
  LISPM_EVAL_CHECK(*b != '#' || !is_lex, LISPM_SYM_NIL, lex_error);
  unsigned assignable = *b == '#' || *b == ':' ? 0 : 2;
  entry[0] = ((M.tp - M.strings) << 2) | assignable;
  entry[1] = assignable ? PARSE_SYM_UNBOUND : lit;
  while (b != e)
    *M.tp++ = *b++;
  *M.tp++ = 0;
  while ((M.tp - M.strings) & 3)
    M.tp++;
  return lit;
}

/* builtins support */
static const struct Builtin *builtin(Sym lit) {
  if (!lispm_sym_is_literal(lit)) return 0;
  Sym val = htable_entry_get_assoc(lit);
  return lispm_sym_is_builtin_sym(val) ? M.builtins + lispm_builtin_sym_offs(val) : 0;
}

/* lexer */
#include "lexer.inc.h"

#define TOK_SYM(c) (((c) << 8) | 19u)
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
      } else if (c == '#') {
        token_begin = M.pc, state = S_ATOM;
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
  return htable_ensure(token_begin, M.pc, 1);
lex_num:
  if (token_val != 0 && *token_begin == '0') goto lex_fail;
  return lispm_make_shortnum(token_val);
}
/* parser */
static void parse_frame_enter(void) {
  ++M.frame_depth;
  M.parse_frame = C(LISPM_SYM_NIL, M.parse_frame);
}
static void parse_frame_shadow_append(Sym lit, Sym old_assoc) {
  Sym *frame = lispm_st_obj_unpack(M.parse_frame);
  frame[0] = C(C(lit, old_assoc), frame[0]);
}
static void parse_frame_bind(Sym lit) {
  LISPM_ASSERT(lispm_sym_is_literal(lit));
  if (!htable_entry_is_assignable(lit)) goto frame_bind_fail;
  Sym new_assoc = PARSE_SYM_BOUND(M.frame_depth), old_assoc = htable_entry_set_assoc(lit, new_assoc);
  if (old_assoc == new_assoc) goto frame_bind_fail;
  return parse_frame_shadow_append(lit, old_assoc);
frame_bind_fail:
  LISPM_EVAL_CHECK(0, lit, illegal_bind, lit);
}
static void parse_frame_use(Sym lit) {
  LISPM_ASSERT(lispm_sym_is_literal(lit));
  if (!htable_entry_is_assignable(lit)) return;
  Sym old_assoc = htable_entry_get_assoc(lit);
  LISPM_EVAL_CHECK(old_assoc != PARSE_SYM_UNBOUND, lit, unbound_symbol, lit);
  if (old_assoc == PARSE_SYM_BOUND(M.frame_depth) || old_assoc == PARSE_SYM_FREE(M.frame_depth)) return;
  parse_frame_shadow_append(lit, htable_entry_set_assoc(lit, PARSE_SYM_FREE(M.frame_depth)));
}
static Sym parse_frame_leave(void) {
  Sym *frame = lispm_st_obj_unpack(M.parse_frame), shadow = frame[0], bound = PARSE_SYM_BOUND(M.frame_depth--);
  M.parse_frame = frame[1];

  Sym captures = LISPM_SYM_NIL, *cons, var, old_assoc;
  while (!lispm_sym_is_nil(shadow)) {
    cons = lispm_st_obj_unpack(shadow), var = cons[0], shadow = cons[1];
    cons = lispm_st_obj_unpack(var), var = cons[0], old_assoc = cons[1];
    if (htable_entry_set_assoc(var, old_assoc) == bound) continue;
    captures = C(var, captures);
    parse_frame_use(var);
  }
  return captures;
}

static Sym SYM_QUOTE; /* initialized during builtin initialization */
static Sym parse(Sym tok) {
  if (lispm_sym_is_atom(tok)) return tok;
  if (tok == TOK_QUOTE) return C(SYM_QUOTE, C(parse(lex()), LISPM_SYM_NIL));

  LISPM_EVAL_CHECK(tok == TOK_LPAREN, tok, parse_error, tok);
  if ((tok = lex()) == TOK_RPAREN) return LISPM_SYM_NIL;

  Sym res = C(parse(tok), LISPM_SYM_NIL);
  while ((tok = lex()) != TOK_RPAREN)
    res = C(parse(tok), res);
  return list_reverse_inplace(res);
}
static Sym sema(Sym syn);

static Sym sema_quote(Sym args) {
  Sym arg;
  LISPM_EVAL_CHECK(lispm_list_scan(&arg, args, 1) == 1, args, panic, "quote expects exactly one argument, got: ", args);
  return arg;
}
static Sym sema_con(Sym brans) {
  Sym res = LISPM_SYM_NIL, conact[2], *cons, bran;
  while (!lispm_sym_is_nil(brans)) {
    cons = lispm_st_obj_unpack(brans), bran = cons[0], brans = cons[1];
    LISPM_EVAL_CHECK(lispm_list_scan(conact, bran, 2) == 2, bran, panic,
                     "conditional branch must have form (condition action), got: ", bran);
    res = C(C(sema(conact[0]), sema(conact[1])), res);
  }
  return list_reverse_inplace(res);
}
static Sym sema_lambda(Sym def) {
  Sym argsbody[2];
  LISPM_EVAL_CHECK(lispm_list_scan(argsbody, def, 2) == 2, def, panic,
                   "lambda must provide (lambda (args...) body), got: ", def);
  parse_frame_enter();
  Sym it = argsbody[0], *cons, arg;
  while (lispm_sym_is_cons(it)) {
    cons = lispm_st_obj_unpack(it), arg = cons[0], it = cons[1];
    LISPM_EVAL_CHECK(lispm_sym_is_literal(arg), arg, parse_error, arg);
    parse_frame_bind(arg);
  }
  LISPM_EVAL_CHECK(lispm_sym_is_nil(it), argsbody[0], panic, "list of formal arguments expected, got: ", argsbody[0]);
  Sym body = sema(argsbody[1]), captures = parse_frame_leave();
  Sym res = lispm_st_obj_alloc(LISPM_ST_OBJ_LAMBDA);
  M.sp[0] = captures, M.sp[1] = argsbody[0], M.sp[2] = body;
  return res;
}
static Sym sema_let(Sym def) {
  Sym asgnsexpr[2], asgns = LISPM_SYM_NIL, expr;
  LISPM_EVAL_CHECK(lispm_list_scan(asgnsexpr, def, 2) == 2, def, panic,
                   "let must define a list of assignments followed by expression, got: ", def);
  Sym it = asgnsexpr[0], *cons, asgn, nameval[2];
  while (lispm_sym_is_cons(it)) {
    cons = lispm_st_obj_unpack(it), asgn = cons[0], it = cons[1];
    LISPM_EVAL_CHECK(lispm_list_scan(nameval, asgn, 2) == 2 && lispm_sym_is_literal(nameval[0]), asgn, panic,
                     "assignment must have form (name expr), got: ", asgn);
    asgns = C(C(nameval[0], sema(nameval[1])), asgns);
    parse_frame_enter();
    parse_frame_bind(nameval[0]);
  }
  expr = sema(asgnsexpr[1]);
  for (Sym it = asgns; it != LISPM_SYM_NIL; it = lispm_st_obj_unpack(it)[1])
    parse_frame_leave();
  return C(list_reverse_inplace(asgns), expr);
}
Sym lispm_parse_quote(const char *pc, const char *pc_end) {
  LISPM_ASSERT(M.program <= pc && pc <= pc_end && pc_end <= M.program_end);
  const char *old_pc_end = M.program_end;
  M.pc = pc;
  M.program_end = pc_end;
  Sym res = parse(lex());
  M.program_end = old_pc_end;
  return res;
}
static Sym sema(Sym syn) {
  if (lispm_sym_is_atom(syn)) {
    if (lispm_sym_is_literal(syn)) parse_frame_use(syn);
    return syn;
  }
  LISPM_ASSERT(lispm_sym_is_cons(syn));
  Sym *cons = lispm_st_obj_unpack(syn), form = cons[0], args = cons[1];

  const struct Builtin *bi = builtin(form);
  if (bi && bi->sema) return C(form, bi->sema(args));

  LISPM_EVAL_CHECK(lispm_sym_is_literal(form) || lispm_sym_is_cons(form), form, panic,
                   "function expected, got: ", form);
  Sym res = C(sema(form), LISPM_SYM_NIL), arg;
  while (!lispm_sym_is_nil(args)) {
    cons = lispm_st_obj_unpack(args), arg = cons[0], args = cons[1];
    res = C(sema(arg), res);
  }
  return list_reverse_inplace(res);
}

/* eval */
static Sym eval(Sym e);
static Sym evquote(Sym arg) { return arg; }
static Sym evlambda(Sym lambda) {
  Sym *proto = lispm_st_obj_unpack(lambda), captures = LISPM_SYM_NIL;
  for (Sym it = proto[0], *cons, name; it != LISPM_SYM_NIL;) {
    cons = lispm_st_obj_unpack(it), name = cons[0], it = cons[1];
    Sym assoc = htable_entry_get_assoc(name);
    LISPM_ASSERT(assoc != PARSE_SYM_UNBOUND);
    captures = C(C(name, assoc), captures);
  }
  Sym res = lispm_st_obj_alloc(LISPM_ST_OBJ_LAMBDA);
  M.sp[0] = captures, M.sp[1] = proto[1], M.sp[2] = proto[2];
  return res;
}
static Sym evcon(Sym brans) {
  Sym *cons, bran;
  while (!lispm_sym_is_nil(brans)) {
    cons = lispm_st_obj_unpack(brans), bran = cons[0], brans = cons[1];
    cons = lispm_st_obj_unpack(bran);
    if (!lispm_sym_is_nil(eval(cons[0]))) return eval(cons[1]);
  }
  LISPM_EVAL_CHECK(0, LISPM_SYM_NIL, panic, "condition branches exhausted", LISPM_SYM_NIL);
}
static Sym evlet(Sym let) {
  Sym *def = lispm_st_obj_unpack(let), shadow = LISPM_SYM_NIL;
  for (Sym it = def[0], asgn, *cons; it != LISPM_SYM_NIL;) {
    cons = lispm_st_obj_unpack(it), asgn = cons[0], it = cons[1];
    cons = lispm_st_obj_unpack(asgn);
    shadow = htable_shadow_append(shadow, cons[0], eval(cons[1]));
  }
  Sym res = eval(def[1]);
  htable_shadow_rollback(shadow);
  return res;
}
static Sym evlis(Sym li) {
  Sym res = LISPM_SYM_NIL, expr, *cons;
  while (!lispm_sym_is_nil(li)) {
    cons = lispm_st_obj_unpack(li), expr = cons[0], li = cons[1];
    res = C(eval(expr), res);
  }
  return list_reverse_inplace(res);
}
static Sym evapply(Sym expr) {
  Sym f, fn = LISPM_SYM_NIL, args, *cons;
  cons = lispm_st_obj_unpack(expr), f = cons[0], args = cons[1];
  LISPM_ASSERT(lispm_sym_is_atom(f) || lispm_sym_is_cons(f));

  const struct Builtin *bi = builtin(f);
  LISPM_EVAL_CHECK(bi || lispm_sym_is_lambda(fn = eval(f)), f, panic, "a function expected, got: ", f);

  if (!bi || !bi->sema) args = evlis(args); /* not a special form, evaluate arguments */
  LISPM_TRACE(apply_enter, f, fn, args);

  if (bi) return bi->eval(args);
  LISPM_ASSERT(lispm_sym_is_lambda(fn));
  Sym *lambda = lispm_st_obj_unpack(fn), shadow = LISPM_SYM_NIL;
  for (Sym it = lambda[0], *cons, asgn; it != LISPM_SYM_NIL;) {
    cons = lispm_st_obj_unpack(it), asgn = cons[0], it = cons[1];
    cons = lispm_st_obj_unpack(asgn);
    shadow = htable_shadow_append(shadow, cons[0], cons[1]);
  }

  Sym argf = lambda[1], argv = args, name, value;
  while (argf != LISPM_SYM_NIL && argv != LISPM_SYM_NIL) {
    cons = lispm_st_obj_unpack(argf), name = cons[0], argf = cons[1];
    cons = lispm_st_obj_unpack(argv), value = cons[0], argv = cons[1];
    shadow = htable_shadow_append(shadow, name, value);
  }
  LISPM_EVAL_CHECK(lispm_sym_is_nil(argf) && lispm_sym_is_nil(argv), fn, panic,
                   "number of formal arguments does not match the number of passed arguments: ", fn);
  Sym res = eval(lambda[2]);
  htable_shadow_rollback(shadow);
  return res;
}
static Sym eval(Sym syn) {
  if (lispm_sym_is_nil(syn) || lispm_sym_is_shortnum(syn)) return syn;
  if (lispm_sym_is_literal(syn)) {
    const struct Builtin *bi = builtin(syn);
    LISPM_EVAL_CHECK(!bi || !bi->sema, syn, panic, "special form cannot be used as a value, got: ", syn);
    return htable_entry_get_assoc(syn);
  }
  unsigned mark = M.sp - M.stack;
  Sym res = gc(evapply(syn), mark);
  LISPM_TRACE(apply_leave);
  return res;
}

/* API */
Sym lispm_eval(const char *pc, const char *pc_end) { return eval(sema(lispm_parse_quote(pc, pc_end))); }
static inline int lispm_is_valid_result(Sym e) {
  return lispm_sym_is_nil(e) || lispm_sym_is_atom(e) || lispm_sym_is_cons(e);
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
  M.htable_index_size = (M.htable_end - M.htable) >> 1;
  M.htable_index_shift = __builtin_clz(M.htable_index_size) + 1;
  M.parse_frame = LISPM_SYM_NIL;
  M.frame_depth = 1;

  int i = 0;
  for (const struct Builtin *bi = M.builtins; bi->name; ++bi, ++i) {
    const char *n = bi->name;
    Sym s = htable_ensure(n, n + __builtin_strlen(n), 0);
    if (htable_entry_is_assignable(s)) {
      unsigned *entry = M.htable + lispm_literal_ht_offs(s);
      entry[0] &= ~2u;
      entry[1] = LISPM_MAKE_BUILTIN_SYM(i);
    }
    if (bi->store) *bi->store = s;
  }
}

Sym lispm_exec(void) {
  LISPM_ASSERT(M.htable_index_size != 0); /* check that the machine has been initialized */
  lispm_rt_try(lispm_main);
  return M.stack[0];
}

static Sym SYM_T;
static Sym LIST(Sym a) { return a; }
static Sym CONS(Sym a) {
  Sym xy[2];
  LISPM_EVAL_CHECK(lispm_list_scan(xy, a, 2) == 2, a, panic, "two arguments expected, got: ", a);
  return C(xy[0], xy[1]);
}
static Sym CONS_UNPACK(Sym a, unsigned offs) {
  Sym arg;
  LISPM_EVAL_CHECK(lispm_list_scan(&arg, a, 1) == 1 && lispm_sym_is_cons(arg), a, panic,
                   "single cons-arguments expected, got: ", a);
  return lispm_st_obj_unpack(arg)[offs];
}
static Sym CAR(Sym a) { return CONS_UNPACK(a, 0); }
static Sym CDR(Sym a) { return CONS_UNPACK(a, 1); }
static Sym ATOM(Sym a) {
  Sym arg;
  LISPM_EVAL_CHECK(lispm_list_scan(&arg, a, 1) == 1, a, panic, "single arguments expected, got: ", a);
  return lispm_sym_is_atom(arg) ? SYM_T : LISPM_SYM_NIL;
}
static Sym EQ(Sym a) {
  Sym xy[2];
  LISPM_EVAL_CHECK(lispm_list_scan(xy, a, 2) == 2, a, panic, "two arguments expected, got: ", a);
  return lispm_sym_is_atom(xy[0]) && xy[0] == xy[1] ? SYM_T : LISPM_SYM_NIL;
}
static Sym PANIC(Sym a) { LISPM_EVAL_CHECK(0, a, panic, "user panic: ", a); }

static const struct Builtin LISPM_CORE_BUILTINS[]
    __attribute__((section(".lispm.rodata.builtins.core"), aligned(16), used)) = {
        {"#t", 0, 0, &SYM_T},
        {"#err!", 0, 0, &SYM_ERR},
        {"quote", evquote, sema_quote, &SYM_QUOTE},
        {"cond", evcon, sema_con},
        {"lambda", evlambda, sema_lambda},
        {"let", evlet, sema_let},
        {"atom?", ATOM},
        {"eq?", EQ},
        {"list", LIST},
        {"cons", CONS},
        {"car", CAR},
        {"cdr", CDR},
        {"panic!", PANIC},
};
