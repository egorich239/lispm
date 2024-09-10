#include <liblispm/lispm.h>

#include <liblispm/builtins.h>
#include <liblispm/obj.h>
#include <liblispm/rt.h>
#include <liblispm/trace.h>
#include <liblispm/types.h>

/**/
#include <liblispm/internal-macros.h>
#include <liblispm/internal-obj.h>

extern struct LispmBuiltin lispm_builtins_start[];
extern struct LispmBuiltin lispm_builtins_end[];

const struct LispmBuiltin LISPM_SYN[];
enum {
  BUILTIN_ERR_INDEX = 0,
  BUILTIN_APPLY_INDEX = 1,
  BUILTIN_ASSOC_INDEX = 2,
  BUILTIN_QUOTE_INDEX = 3,
  BUILTIN_LAMBDA_INDEX = 4,
};

/* builtins */
Obj lispm_obj_from_builtin(const struct LispmBuiltin *bi) {
  LISPM_ASSERT(lispm_builtins_start <= bi && bi < lispm_builtins_end);
  return M.stack_end[~(bi - lispm_builtins_start)];
}

/* error reporting */
__attribute__((noreturn)) void lispm_panic0(Obj ctx) {
  LISPM_ASSERT(M.stack);
  M.stack[1] = ctx;
  lispm_rt_throw(M.stack_end[~BUILTIN_ERR_INDEX]);
}
Obj lispm_return0(Obj obj) { return C(NIL, obj); }

/* list functions */
unsigned lispm_list_scan(Obj *out, Obj li, unsigned limit) {
  Obj it = li, val;
  unsigned cntr = 0;
  while (lispm_obj_is_cons(it) && cntr < limit) {
    C_UNPACK(it, val, it);
    out[cntr++] = val;
  }
  LISPM_ASSERT(lispm_obj_is_nil(it) || lispm_obj_is_cons(it));
  return lispm_obj_is_nil(it) ? cntr : ~0u;
}

static Obj list_reverse_inplace(Obj li, unsigned gc_offset) {
  /* not a public interface because it actually changes the object state */
  LISPM_ASSERT(lispm_obj_is_nil(li) || lispm_obj_is_st_obj(li));
  Obj cur = li, prev = NIL, next, *cons;
  while (!lispm_obj_is_nil(cur)) {
    cons = lispm_obj_unpack(cur), next = cons[0];
    cons[0] = lispm_gc_move(prev, gc_offset), prev = cur, cur = next;
  }
  return lispm_gc_move(prev, gc_offset);
}
static Obj list_map(Obj li, Obj (*fn)(Obj)) {
  Obj res = NIL;
  FOR_EACH_C(v, li) { res = C(fn(v), res); }
  return list_reverse_inplace(res, 0);
}

/* stack functions */
static inline Obj *lispm_st_obj_alloc0(unsigned size) {
  LISPM_EVAL_CHECK(M.stack + LISPM_STACK_BOTTOM_OFFSET + size <= M.sp, NIL, oom_stack);
  return M.sp -= size;
}
Obj lispm_obj_alloc0(enum LispmStObjKind k) {
  return lispm_make_st_obj(k, lispm_st_obj_alloc0(lispm_obj_st_size(k)) - M.stack);
}
Obj *lispm_obj_unpack(Obj s) { return M.stack + lispm_obj_st_offs(s); }
static inline unsigned gc_mark(void) { return M.sp - M.stack; }
static Obj gc0(Obj s, LispmObj gc_bound, unsigned gc_offset) {
  TRACE_NATIVE_STACK();
  Obj tail = LISPM_SYM_NIL, *dest;
  while (lispm_obj_is_st_obj(s) && s < gc_bound) {
    unsigned sz = lispm_obj_st_size(s);
    Obj *src = lispm_obj_unpack(s), new_tail = lispm_obj_alloc0(lispm_obj_st_kind(s));
    dest = M.sp, dest[0] = tail, tail = new_tail;
    for (unsigned p = sz - 1; p; --p)
      dest[p] = gc0(src[p], gc_bound, gc_offset);
    s = src[0];
  }
  if (lispm_obj_is_nil(tail)) return s;
  Obj res = list_reverse_inplace(tail, gc_offset);
  dest[0] = gc0(s, gc_bound, gc_offset);
  return res;
}
static Obj gc(Obj root, unsigned high_mark) {
  unsigned low_mark = gc_mark();
  root = gc0(root, lispm_gc_bound(high_mark), lispm_gc_offset(low_mark, high_mark));
  const unsigned lowest_mark = gc_mark();
  LISPM_TRACE(stack_depth, LISPM_TRACE_STACK_OBJECTS, M.stack_end - M.sp);
  while (lowest_mark < low_mark)
    M.stack[--high_mark] = M.stack[--low_mark];
  M.sp = M.stack + high_mark;
  return root;
}

/* htable functions */
static inline unsigned htable_entry_key(Obj s) { return M.htable[lispm_literal_ht_offs(s)]; }
static inline int htable_entry_key_payload(unsigned key) { return key >> 2; }
static inline int htable_entry_is_lvalue(Obj s) {
  LISPM_ASSERT(lispm_obj_is_literal(s));
  return htable_entry_key(s) & LISPM_HTABLE_LITERAL_LVALUE;
}
static inline int htable_entry_is_rvalue(Obj s) {
  LISPM_ASSERT(lispm_obj_is_literal(s));
  return !(htable_entry_key(s) & LISPM_HTABLE_LITERAL_NOT_RVALUE);
}
static inline int htable_entry_key_streq(unsigned key) {
  const char *h = M.strings + htable_entry_key_payload(key);
  return !__builtin_strcmp(h, M.tp);
}
static inline Obj htable_entry_get_assoc(Obj s) {
  LISPM_ASSERT(lispm_obj_is_literal(s));
  return M.htable[lispm_literal_ht_offs(s) + 1];
}
static Obj htable_entry_set_assoc(Obj s, Obj assoc) {
  LISPM_ASSERT(lispm_obj_is_literal(s) && htable_entry_is_lvalue(s));
  Obj *a = M.htable + (lispm_literal_ht_offs(s) + 1), old_assoc = *a;
  return *a = assoc, old_assoc;
}
static unsigned htable_hashf(unsigned seed) {
  unsigned hash = seed;
  for (unsigned char *b = (unsigned char *)M.tp; *b; b++)
    hash = 33 * hash + *b;
  hash ^= (hash >> M.htable_index_shift);
  hash = (2654435769u * hash) >> M.htable_index_shift;
  LISPM_ASSERT(hash < M.htable_index_size);
  return hash;
}
static Obj htable_ensure(unsigned flags) {
  unsigned offset = 5381, *entry;
  Obj lit;
  for (int attempt = 0; attempt < LISPM_CONFIG_HTABLE_LOOKUP_LIMIT; ++attempt) {
    offset = htable_hashf(offset);
    entry = M.htable + (2 * offset);
    lit = lispm_make_literal(2 * offset);
    if (!*entry) goto ensure_insert;                /* empty slot */
    if (htable_entry_key_streq(*entry)) return lit; /* found! */
  }
  return LISPM_HTABLE_EXHAUSTED;

ensure_insert:
  if (flags & LISPM_HTABLE_ENSURE_FORBID_INSERT) return LISPM_HTABLE_NOT_FOUND;
  unsigned selfref = (*M.tp == '#' || *M.tp == ':') ? 1 : 0;
  if (selfref) flags &= ~LISPM_HTABLE_LITERAL_LVALUE;
  entry[0] = ((M.tp - M.strings) << 2) | flags;
  entry[1] = selfref ? lit : LISPM_LEX_UNBOUND;
  while (*M.tp++) {}
  while ((++M.tp - M.strings) & 3) {}
  return lit ^ selfref;
}

/* lexer */
#include <liblispm/lexer.inc.h>

static Obj lex(void) {
  enum { S_ATOM, S_NUM, S_COMMENT, S_INIT } state = S_INIT;
  _Static_assert(LEX_IS_ATOM_SYM(S_ATOM), "S_ATOM must match the category");
  _Static_assert(LEX_IS_DIGIT(S_NUM), "S_NUM must match the category");
  char *tp = M.tp;
  unsigned token_val = 0, flags = LISPM_HTABLE_LITERAL_LVALUE;
  unsigned fc = 0, c, cat;
  for (; tp + 4 < M.strings_end && M.pc < M.pc_end; ++M.pc) {
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
        *tp++ = c, state = S_ATOM;
        flags ^= LISPM_HTABLE_ENSURE_FORBID_INSERT;
        continue;
      }
      if (LEX_IS_TOK(cat)) return ++M.pc, lispm_make_token(c);
      if (LEX_IS_ATOM_SYM(cat)) {
        M.pc--, state = cat, fc = c;
        continue;
      }
      goto lex_fail;
    case S_COMMENT:
      if (c == '\n') state = S_INIT;
      break;
    case S_ATOM:
      if (LEX_IS_ATOM_SYM(cat)) {
        *tp++ = c;
        continue;
      }
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
  if (tp + 4 == M.strings_end) goto lex_fail;
  if (state == S_ATOM) goto lex_atom;
  if (state == S_NUM) goto lex_num;
lex_fail:
  LISPM_EVAL_CHECK(0, NIL, lex_error);
lex_atom:
  *tp++ = 0;
  Obj res = htable_ensure(flags) & ~1u;
  LISPM_EVAL_CHECK(!lispm_obj_is_htable_error(res), res, panic, "could not insert symbol: ", res);
  return res;
lex_num:
  if (token_val != 0 && fc == '0') goto lex_fail;
  return lispm_make_shortnum(token_val);
}
/* parser */
static void lex_frame_enter(void) {
  lispm_frame_depth_inc(&M.frame_depth);
  M.frame = C(NIL, M.frame);
}
static void lex_frame_shadow(Obj lit, Obj old_assoc) {
  Obj old_shadow = C_CAR(M.frame);
  C_CAR(M.frame) = T(lit, old_assoc, old_shadow);
}
static void lex_frame_bind(Obj lit, unsigned mask) {
  LISPM_ASSERT(lispm_obj_is_literal(lit) && (mask == LISPM_LEX_BOUND || mask == LISPM_LEX_BOUNDREC));
  if (!htable_entry_is_lvalue(lit)) goto frame_bind_fail;
  Obj new_assoc = lispm_make_sema(M.frame_depth, mask), old_assoc = htable_entry_set_assoc(lit, new_assoc);
  if (lispm_obj_is_sema_bound(old_assoc) && lispm_obj_sema_depth(old_assoc) == M.frame_depth) goto frame_bind_fail;
  return lex_frame_shadow(lit, old_assoc);
frame_bind_fail:
  LISPM_EVAL_CHECK(0, lit, illegal_bind, lit);
}
static void lex_frame_use(Obj lit) {
  LISPM_ASSERT(lispm_obj_is_literal(lit));
  if (!htable_entry_is_lvalue(lit)) return;
  Obj old_assoc = htable_entry_get_assoc(lit), new_assoc = lispm_make_sema(M.frame_depth, LISPM_LEX_FREE);
  LISPM_EVAL_CHECK(old_assoc != LISPM_LEX_UNBOUND, lit, unbound_symbol, lit);
  if (new_assoc > old_assoc) lex_frame_shadow(lit, htable_entry_set_assoc(lit, new_assoc));
}
static Obj lex_frame_leave(void) {
  Obj shadow;
  C_UNPACK(M.frame, shadow, M.frame);
  lispm_frame_depth_dec(&M.frame_depth);

  Obj captures = NIL;
  FOR_EACH_T(var, old_assoc, shadow) {
    if (!lispm_obj_is_sema_free(htable_entry_set_assoc(var, old_assoc))) continue;
    captures = C(var, captures);
    lex_frame_use(var);
  }
  return captures;
}

static Obj parse(Obj tok) {
  if (lispm_obj_is_atom(tok)) return tok;
  TRACE_NATIVE_STACK();

  if (tok == LISPM_TOK_QUOTE) return C(M.stack_end[~BUILTIN_QUOTE_INDEX], C(lispm_parse_quote0(), NIL));
  LISPM_EVAL_CHECK(tok == LISPM_TOK_LPAREN, tok, parse_error, tok);
  if ((tok = lex()) == LISPM_TOK_RPAREN) return NIL;

  Obj res = C(parse(tok), NIL);
  while ((tok = lex()) != LISPM_TOK_RPAREN)
    res = C(parse(tok), res);
  return list_reverse_inplace(res, 0);
}
Obj lispm_parse_quote0(void) {
  LISPM_ASSERT(M.pc <= M.pc_end);
  return parse(lex());
}

static Obj sema_apply_lambda(Obj proto, Obj args) {
  Obj closure = C(lispm_make_builtin(BUILTIN_LAMBDA_INDEX), proto);
  return C(lispm_make_builtin(BUILTIN_APPLY_INDEX), C(closure, args));
}
static Obj sema(Obj syn);
static Obj sema_quote(Obj args) {
  Obj arg;
  LISPM_EVAL_CHECK(lispm_list_scan(&arg, args, 1) == 1, args, panic, "quote expects exactly one argument, got: ", args);
  return arg;
}
static Obj sema_con(Obj brans) {
  Obj res = NIL, conact[2];
  C_ENSURE(brans, "list of conditional actions expected");
  FOR_EACH_C(bran, brans) {
    LISPM_EVAL_CHECK(lispm_list_scan(conact, bran, 2) == 2, bran, panic,
                     "conditional branch must have form (condition action), got: ", bran);
    res = T(sema(conact[0]), sema(conact[1]), res);
  }
  return list_reverse_inplace(res, 0);
}
static Obj sema_lambda(Obj def) {
  Obj argsbody[2];
  LISPM_EVAL_CHECK(lispm_list_scan(argsbody, def, 2) == 2, def, panic,
                   "lambda must provide (lambda (args...) body), got: ", def);
  lex_frame_enter();
  C_ENSURE(argsbody[0], "list of formal arguments expected")
  FOR_EACH_C(arg, argsbody[0]) {
    LISPM_EVAL_CHECK(lispm_obj_is_literal(arg), arg, parse_error, arg);
    lex_frame_bind(arg, LISPM_LEX_BOUND);
  }
  Obj body = sema(argsbody[1]), captures = lex_frame_leave();
  return T(captures, argsbody[0], body);
}
static Obj sema_let(Obj def) {
  Obj asgnsexpr[2], asgns = NIL, expr, nameval[2];
  LISPM_EVAL_CHECK(lispm_list_scan(asgnsexpr, def, 2) == 2, def, panic,
                   "let must define a list of assignments followed by expression, got: ", def);
  C_ENSURE(asgnsexpr[0], "list of assignments expected")
  FOR_EACH_C(asgn, asgnsexpr[0]) {
    LISPM_EVAL_CHECK(lispm_list_scan(nameval, asgn, 2) == 2 && lispm_obj_is_literal(nameval[0]), asgn, panic,
                     "assignment must have form (name expr), got: ", asgn);
    asgns = T(nameval[0], sema(nameval[1]), asgns);
    lex_frame_enter();
    lex_frame_bind(nameval[0], LISPM_LEX_BOUND);
  }
  expr = sema(asgnsexpr[1]);
  FOR_EACH_T(name, val, asgns) { expr = sema_apply_lambda(T(lex_frame_leave(), C(name, NIL), expr), C(val, NIL)); }
  return expr;
}
static Obj sema_letrec(Obj def) {
  Obj asgnsexpr[2], args = NIL, exprs = NIL, nameval[2];
  LISPM_EVAL_CHECK(lispm_list_scan(asgnsexpr, def, 2) == 2, def, panic,
                   "letrec must define a list of assignments followed by expression, got: ", def);
  lex_frame_enter();
  C_ENSURE(asgnsexpr[0], "list of assignments expected");
  FOR_EACH_C(asgn, asgnsexpr[0]) {
    LISPM_EVAL_CHECK(lispm_list_scan(nameval, asgn, 2) == 2 && lispm_obj_is_literal(nameval[0]), asgn, panic,
                     "assignment must have form (name expr), got: ", asgn);
    args = C(nameval[0], args);
    exprs = C(nameval[1], exprs);
    lex_frame_bind(nameval[0], LISPM_LEX_BOUNDREC);
  }
  Obj vals = list_map(exprs, sema), expr = sema(asgnsexpr[1]), captures = lex_frame_leave();
  return C(T(captures, args, expr), vals);
}
static Obj sema(Obj syn) {
  if (lispm_obj_is_literal(syn)) {
    lex_frame_use(syn);
    return C(lispm_make_builtin(BUILTIN_ASSOC_INDEX), syn);
  }
  if (lispm_obj_is_atom(syn)) return lispm_return0(syn);

  TRACE_NATIVE_STACK();

  LISPM_ASSERT(lispm_obj_is_cons(syn));
  Obj form, args;
  C_UNPACK(syn, form, args);

  if (!lispm_obj_is_literal(form)) goto sema_ret_apply;
  Obj assoc = htable_entry_get_assoc(form);
  if (!lispm_obj_is_builtin(assoc)) goto sema_ret_apply;
  const struct LispmBuiltin *bi = lispm_builtins_start + lispm_obj_builtin_offs(assoc);
  if (bi && bi->sema) return C(assoc, bi->sema(args));
sema_ret_apply:
  return C(lispm_make_builtin(BUILTIN_APPLY_INDEX), list_map(syn, sema));
}
static Obj frame_depth_guard(Obj (*op)(Obj), Obj v) {
  LISPM_ASSERT(lispm_obj_is_shortnum(M.frame_depth));
  unsigned orig_depth = M.frame_depth;
  Obj res = op(v);
  LISPM_ASSERT(orig_depth == M.frame_depth);
  return (void)orig_depth, res;
}

/* eval */
static Obj evframe_get(Obj name) {
  LISPM_ASSERT(lispm_obj_is_literal(name));
  LISPM_EVAL_CHECK(htable_entry_is_rvalue(name), name, panic, "cannot use symbol as a value: ", name);
  Obj res = htable_entry_get_assoc(name);
  if (lispm_obj_is_assoc(res)) res = S_1(lispm_obj_assoc_deref(res));
  LISPM_EVAL_CHECK(res != LISPM_LEX_UNBOUND, name, unbound_symbol, name);
  return res;
}
static void evframe_set(Obj name, Obj val) {
  LISPM_ASSERT(lispm_obj_is_literal(name) && htable_entry_is_lvalue(name));
  Obj res = htable_entry_get_assoc(name);
  if (lispm_obj_is_assoc(res) && S_2(lispm_obj_assoc_deref(res)) == M.frame_depth) {
    S_1(lispm_obj_assoc_deref(res)) = val;
  } else {
    M.frame = P(val, M.frame_depth, res, name, M.frame);
    htable_entry_set_assoc(name, lispm_make_assoc(M.frame));
  }
}
static Obj evframe_enter(void) {
  lispm_frame_depth_inc(&M.frame_depth);
  Obj old_frame = M.frame;
  return M.frame = NIL, old_frame;
}
static void evframe_leave(Obj old_frame) {
  while (!lispm_obj_is_nil(M.frame)) {
    htable_entry_set_assoc(S_4(M.frame), S_3(M.frame));
    M.frame = S_NEXT(M.frame);
  }
  M.frame = old_frame;
  lispm_frame_depth_dec(&M.frame_depth);
}
static Obj eval(Obj e);
static Obj evquote(Obj arg) { return lispm_return0(arg); }
static Obj evassoc(Obj arg) { return lispm_return0(evframe_get(arg)); }
static Obj evlambda(Obj lambda) {
  Obj names, args, body, captures = NIL;
  T_UNPACK(lambda, names, args, body);
  FOR_EACH_C(name, names) { captures = T(name, evframe_get(name), captures); }
  return lispm_return0(T(captures, args, body));
}
static Obj evcon(Obj brans) {
  FOR_EACH_T(cond, act, brans) {
    if (!lispm_obj_is_nil(eval(cond))) return act;
  }
  LISPM_EVAL_CHECK(0, NIL, panic, "condition branches exhausted", NIL);
}
static Obj evlet(Obj arg) { return arg; }
static Obj evletrec(Obj arg) {
  Obj lambda, exprs;
  C_UNPACK(arg, lambda, exprs);
  FOR_EACH_C(name, S_2(lambda)) { evframe_set(name, LISPM_LEX_UNBOUND); }
  return sema_apply_lambda(lambda, exprs);
}
static Obj evapply(Obj expr) {
  Obj es = list_map(expr, eval), f, args;
  C_UNPACK(es, f, args);
  if (lispm_obj_is_builtin(f)) return es;

  LISPM_EVAL_CHECK(lispm_obj_is_triplet(f), f, panic, "a function expected, got: ", f);
  Obj captures, argf, body;
  T_UNPACK(f, captures, argf, body);
  FOR_EACH_T(name, val, captures) { evframe_set(name, val); }
  Obj argv = args, name, value;
  while (argf != NIL && argv != NIL) { /* arguments */
    C_UNPACK(argf, name, argf);
    C_UNPACK(argv, value, argv);
    evframe_set(name, value);
  }
  LISPM_EVAL_CHECK(lispm_obj_is_nil(argf) && lispm_obj_is_nil(argv), f, panic,
                   "number of formal arguments does not match the number of passed arguments: ", f);
  return body;
}
static Obj eval(Obj syn) {
  TRACE_NATIVE_STACK();
  unsigned mark = M.sp - M.stack;
  Obj old_frame = evframe_enter(), res;
  for (;;) {
    Obj form, arg;
    C_UNPACK(syn, form, arg);
    if (lispm_obj_is_nil(form)) {
      res = arg;
      break;
    }

    LISPM_ASSERT(lispm_obj_is_builtin(form));
    const struct LispmBuiltin *bi = lispm_builtins_start + lispm_obj_builtin_offs(form);
    LISPM_EVAL_CHECK(bi->eval, form, panic, "a function expected, got: ", form);

    syn = bi->eval(arg);
  }
  evframe_leave(old_frame);
  return gc(res, mark);
}

/* API */
Obj lispm_eval0(void) {
  unsigned mark = gc_mark();
  Obj program = frame_depth_guard(sema, lispm_parse_quote0());
  Obj res = frame_depth_guard(eval, program);
  return gc(res, mark);
}

int lispm_init(void) {
  LISPM_ASSERT(lispm_is_valid_config());
  M.sp = M.stack_end;
  M.tp = M.strings + 4; /* we use zero as sentinel value for missing entry in htable,
                           hence no value can have a zero offset into the strings table */
  M.htable_index_size = (M.htable_end - M.htable) >> 1;
  M.htable_index_shift = __builtin_clz(M.htable_index_size) + 1;
  M.frame = NIL;
  M.frame_depth = lispm_make_shortnum(1);

  const unsigned bilen = lispm_builtins_end - lispm_builtins_start;
  if (M.sp - (M.stack + LISPM_STACK_BOTTOM_OFFSET) < bilen) return 0;
  for (int i = 0; i < bilen; ++i) {
    const struct LispmBuiltin *bi = lispm_builtins_start + i;
    const char *n = bi->name, *ns = n;
    if (!n) continue;
    for (char *nt = M.tp; (*nt++ = *ns++);)
      if (nt + 4 == M.strings_end) return 0;
    Obj s = htable_ensure(bi->flags);
    if (lispm_obj_is_htable_error(s)) return 0;
    /* set_assoc below asserts, because the value is marked not lvalue */
    if (!(s & 1u)) M.htable[lispm_literal_ht_offs(s) + 1] = lispm_make_builtin(i);
    M.stack_end[~i] = s & ~1u;
  }
  M.sp -= bilen;
  return 1;
}

static Obj trycatch(unsigned mode) {
  LISPM_ASSERT(M.htable_index_size != 0); /* check that the machine has been initialized */
  M.stack_bottom_mark = lispm_rt_stack_mark();
  return M.stack[0] = lispm_rt_try(mode == 1 ? lispm_parse_quote0 : lispm_eval0);
}
Obj lispm_parse_quote(void) { return trycatch(1); }
Obj lispm_eval(void) { return trycatch(2); }

static Obj PANIC(Obj a) { LISPM_EVAL_CHECK(0, a, panic, "user panic: ", a); }

const struct LispmBuiltin LISPM_SYN[] __attribute__((section(".lispm.rodata.builtins.core"), aligned(16), used)) = {
    {"#err!"},
    {"(apply)", evapply, 0, LISPM_HTABLE_LITERAL_NOT_RVALUE},
    {"(assoc)", evassoc, 0, LISPM_HTABLE_LITERAL_NOT_RVALUE},
    {"quote", evquote, sema_quote, LISPM_HTABLE_LITERAL_NOT_RVALUE},
    {"lambda", evlambda, sema_lambda, LISPM_HTABLE_LITERAL_NOT_RVALUE},
    {"let", evlet, sema_let, LISPM_HTABLE_LITERAL_NOT_RVALUE},
    {"letrec", evletrec, sema_letrec, LISPM_HTABLE_LITERAL_NOT_RVALUE},
    {"cond", evcon, sema_con, LISPM_HTABLE_LITERAL_NOT_RVALUE},
    {"panic!", PANIC},
};
