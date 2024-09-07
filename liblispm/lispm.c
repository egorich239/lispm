#include <liblispm/lispm.h>

#include <liblispm/builtins.h>
#include <liblispm/obj.h>
#include <liblispm/rt.h>
#include <liblispm/trace.h>
#include <liblispm/types.h>

/**/
#include <liblispm/interal-macros.h>

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
  M.stack[0] = M.stack_end[~BUILTIN_ERR_INDEX];
  M.stack[1] = ctx;
  lispm_rt_throw();
}
LispmObj lispm_return0(LispmObj obj) { return C(NIL, obj); }

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

static Obj list_reverse_inplace(Obj li, unsigned offset) {
  /* not a public interface because it actually changes the object state */
  LISPM_ASSERT(lispm_obj_is_nil(li) || lispm_obj_is_st_obj(li));
  Obj cur = li, prev = NIL, next, *cons;
  while (!lispm_obj_is_nil(cur)) {
    cons = lispm_obj_unpack(cur), next = cons[0];
    cons[0] = lispm_obj_st_move(prev, offset), prev = cur, cur = next;
  }
  return lispm_obj_st_move(prev, offset);
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
static Obj gc0(Obj s, unsigned high_mark, unsigned offset) {
  TRACE_NATIVE_STACK();
  Obj tail = LISPM_SYM_NIL, *dest;
  while (lispm_obj_is_st_obj(s) && lispm_obj_st_offs(s) < high_mark) {
    unsigned sz = lispm_obj_st_size(s);
    Obj *src = lispm_obj_unpack(s), new_tail = lispm_obj_alloc0(lispm_obj_st_kind(s));
    dest = M.sp, dest[0] = tail, tail = new_tail;
    for (unsigned p = sz - 1; p; --p)
      dest[p] = gc0(src[p], high_mark, offset);
    s = src[0];
  }
  if (lispm_obj_is_nil(tail)) return s;
  Obj res = list_reverse_inplace(tail, offset);
  dest[0] = gc0(s, high_mark, offset);
  return res;
}
static Obj gc(Obj root, unsigned high_mark) {
  unsigned low_mark = M.sp - M.stack;
  root = gc0(root, high_mark, high_mark - low_mark);
  const unsigned lowest_mark = M.sp - M.stack;
  LISPM_TRACE(stack_depth, LISPM_TRACE_STACK_OBJECTS, M.stack_end - M.sp);
  while (lowest_mark < low_mark)
    M.stack[--high_mark] = M.stack[--low_mark];
  M.sp = M.stack + high_mark;
  return root;
}

/* htable functions */
#define PARSE_SYM_FREE(depth)       (((depth) << 4) | 7u)
#define PARSE_SYM_BOUND(depth, rec) (((depth) << 4) | 11u | ((rec & 1u) << 2))
enum { PARSE_SYM_UNBOUND = PARSE_SYM_BOUND(0, 0) };

static inline unsigned htable_entry_key(Obj s) { return M.htable[lispm_literal_ht_offs(s)]; }
static inline int htable_entry_key_payload(unsigned key) { return key >> 2; }
static inline int htable_entry_is_lvalue(Obj s) {
  LISPM_ASSERT(lispm_obj_is_literal(s));
  return (htable_entry_key(s) & 2) == 2;
}
static inline int htable_entry_is_rvalue(Obj s) {
  LISPM_ASSERT(lispm_obj_is_literal(s));
  return (htable_entry_key(s) & 1) == 0;
}
static inline int htable_entry_key_streq(unsigned key, const char *b, const char *e) {
  const char *h = M.strings + htable_entry_key_payload(key);
  while (b != e)
    if (*b++ != *h++) return 0;
  return !*h;
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
static unsigned htable_hashf(const char *b, const char *e, unsigned seed) {
  unsigned hash = seed;
  while (b != e)
    hash = 33 * hash + ((unsigned char)*b++);
  hash ^= (hash >> M.htable_index_shift);
  hash = (2654435769u * hash) >> M.htable_index_shift;
  LISPM_ASSERT(hash < M.htable_index_size);
  return hash;
}
static Obj htable_ensure(const char *b, const char *e, int is_lex) {
  unsigned offset = 5381, *entry;
  Obj lit;
  for (int attempt = 0; attempt < LISPM_CONFIG_HTABLE_LOOKUP_LIMIT; ++attempt) {
    offset = htable_hashf(b, e, offset);
    entry = M.htable + (2 * offset);
    lit = lispm_make_literal(2 * offset);
    if (!*entry) goto ensure_insert;                      /* empty slot */
    if (htable_entry_key_streq(*entry, b, e)) return lit; /* found! */
  }
  LISPM_EVAL_CHECK(0, NIL, oom_htable);

ensure_insert:
  LISPM_EVAL_CHECK(M.tp + (e - b + 1) <= M.strings_end, NIL, oom_strings);
  LISPM_EVAL_CHECK(*b != '#' || !is_lex, NIL, lex_error);
  unsigned lvalue = *b == '#' || *b == ':' ? 0 : 2;
  entry[0] = ((M.tp - M.strings) << 2) | lvalue;
  entry[1] = lvalue ? PARSE_SYM_UNBOUND : lit;
  while (b != e)
    *M.tp++ = *b++;
  *M.tp++ = 0;
  while ((M.tp - M.strings) & 3)
    M.tp++;
  return lit;
}

/* lexer */
#include <liblispm/lexer.inc.h>

#define TOK_SYM(c) (((c) << 8) | 19u)
#define TOK_LPAREN TOK_SYM('(')
#define TOK_RPAREN TOK_SYM(')')
#define TOK_QUOTE  TOK_SYM('\'')

static Obj lex(void) {
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
  LISPM_EVAL_CHECK(0, NIL, lex_error);
lex_atom:
  return htable_ensure(token_begin, M.pc, 1);
lex_num:
  if (token_val != 0 && *token_begin == '0') goto lex_fail;
  return lispm_make_shortnum(token_val);
}
/* parser */
static void lex_frame_enter(void) {
  ++M.frame_depth;
  M.frame = C(NIL, M.frame);
}
static void lex_frame_shadow(Obj lit, Obj old_assoc) {
  Obj old_shadow = C_CAR(M.frame);
  C_CAR(M.frame) = T(lit, old_assoc, old_shadow);
}
static void lex_frame_bind(Obj lit, unsigned rec) {
  LISPM_ASSERT(lispm_obj_is_literal(lit));
  if (!htable_entry_is_lvalue(lit)) goto frame_bind_fail;
  Obj new_assoc = PARSE_SYM_BOUND(M.frame_depth, rec), old_assoc = htable_entry_set_assoc(lit, new_assoc);
  if (old_assoc == new_assoc) goto frame_bind_fail;
  return lex_frame_shadow(lit, old_assoc);
frame_bind_fail:
  LISPM_EVAL_CHECK(0, lit, illegal_bind, lit);
}
static void lex_frame_use(Obj lit) {
  LISPM_ASSERT(lispm_obj_is_literal(lit));
  if (!htable_entry_is_lvalue(lit)) return;
  Obj old_assoc = htable_entry_get_assoc(lit);
  LISPM_EVAL_CHECK(old_assoc != PARSE_SYM_UNBOUND, lit, unbound_symbol, lit);
  if (old_assoc == PARSE_SYM_BOUND(M.frame_depth, 0) || old_assoc == PARSE_SYM_FREE(M.frame_depth) ||
      (old_assoc & 15u) == 15u)
    return;
  lex_frame_shadow(lit, htable_entry_set_assoc(lit, PARSE_SYM_FREE(M.frame_depth)));
}
static Obj lex_frame_leave(void) {
  Obj shadow, bound = PARSE_SYM_BOUND(M.frame_depth--, 0);
  C_UNPACK(M.frame, shadow, M.frame);

  Obj captures = NIL;
  FOR_EACH_T(var, old_assoc, shadow) {
    Obj cur = htable_entry_set_assoc(var, old_assoc);
    if (cur == bound || (cur & 15u) == 15u) continue;
    captures = C(var, captures);
    lex_frame_use(var);
  }
  return captures;
}

static Obj parse(Obj tok) {
  if (lispm_obj_is_atom(tok)) return tok;
  TRACE_NATIVE_STACK();

  if (tok == TOK_QUOTE) return C(M.stack_end[~BUILTIN_QUOTE_INDEX], C(parse(lex()), NIL));
  LISPM_EVAL_CHECK(tok == TOK_LPAREN, tok, parse_error, tok);
  if ((tok = lex()) == TOK_RPAREN) return NIL;

  Obj res = C(parse(tok), NIL);
  while ((tok = lex()) != TOK_RPAREN)
    res = C(parse(tok), res);
  return list_reverse_inplace(res, 0);
}
Obj lispm_parse_quote0(const char *pc, const char *pc_end) {
  LISPM_ASSERT(M.program <= pc && pc <= pc_end && pc_end <= M.program_end);
  const char *old_pc_end = M.program_end;
  M.pc = pc;
  M.program_end = pc_end;
  Obj res = parse(lex());
  M.program_end = old_pc_end;
  return res;
}

static Obj sema_apply_lambda(Obj proto, Obj args) {
  Obj closure = C(LISPM_MAKE_BUILTIN_SYM(BUILTIN_LAMBDA_INDEX), proto);
  return C(LISPM_MAKE_BUILTIN_SYM(BUILTIN_APPLY_INDEX), C(closure, args));
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
    lex_frame_bind(arg, 0);
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
    lex_frame_bind(nameval[0], 0);
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
    lex_frame_bind(nameval[0], 1);
  }
  Obj vals = list_map(exprs, sema), expr = sema(asgnsexpr[1]), captures = lex_frame_leave();
  return C(T(captures, args, expr), vals);
}
static Obj sema(Obj syn) {
  if (lispm_obj_is_nil(syn) || lispm_obj_is_shortnum(syn)) return C(LISPM_MAKE_BUILTIN_SYM(BUILTIN_QUOTE_INDEX), syn);
  if (lispm_obj_is_literal(syn)) {
    lex_frame_use(syn);
    return C(LISPM_MAKE_BUILTIN_SYM(BUILTIN_ASSOC_INDEX), syn);
  }

  TRACE_NATIVE_STACK();

  LISPM_ASSERT(lispm_obj_is_cons(syn));
  Obj form, args;
  C_UNPACK(syn, form, args);

  if (!lispm_obj_is_literal(form)) goto sema_ret_apply;
  Obj assoc = htable_entry_get_assoc(form);
  if (!lispm_obj_is_builtin_sym(assoc)) goto sema_ret_apply;
  const struct LispmBuiltin *bi = lispm_builtins_start + lispm_builtin_sym_offs(assoc);
  if (bi && bi->sema) return C(assoc, bi->sema(args));
sema_ret_apply:
  return C(LISPM_MAKE_BUILTIN_SYM(BUILTIN_APPLY_INDEX), list_map(syn, sema));
}

/* eval */
static Obj evframe_get(Obj name) {
  LISPM_ASSERT(lispm_obj_is_literal(name));
  LISPM_EVAL_CHECK(htable_entry_is_rvalue(name), name, panic, "cannot use symbol as a value: ", name);
  Obj res = htable_entry_get_assoc(name);
  if ((res & 15u) == 15u) res = S_1(res & ~1u);
  LISPM_EVAL_CHECK(res != PARSE_SYM_UNBOUND, name, unbound_symbol, name);
  return res;
}
static void evframe_set(Obj name, Obj val) {
  LISPM_ASSERT(lispm_obj_is_literal(name) && htable_entry_is_lvalue(name));
  Obj res = htable_entry_get_assoc(name);
  if ((res & 15u) == 15u && S_2(res & ~1u) == lispm_make_shortnum(M.frame_depth)) {
    S_1(res & ~1u) = val;
  } else {
    M.frame = P(val, lispm_make_shortnum(M.frame_depth), res, name, M.frame);
    htable_entry_set_assoc(name, M.frame | 15u);
  }
}
static Obj evframe_enter(void) {
  M.frame_depth++;
  Obj old_frame = M.frame;
  return M.frame = NIL, old_frame;
}
static void evframe_leave(Obj old_frame) {
  while (!lispm_obj_is_nil(M.frame)) {
    htable_entry_set_assoc(S_4(M.frame), S_3(M.frame));
    M.frame = S_NEXT(M.frame);
  }
  M.frame = old_frame;
  M.frame_depth--;
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
  FOR_EACH_C(name, S_2(lambda)) { evframe_set(name, PARSE_SYM_UNBOUND); }
  return sema_apply_lambda(lambda, exprs);
}
static Obj evapply(Obj expr) {
  Obj es = list_map(expr, eval), f, args;
  C_UNPACK(es, f, args);
  if (lispm_obj_is_builtin_sym(f)) return es;

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
  Obj old_frame = evframe_enter();
  for (;;) {
    Obj form, arg;
    C_UNPACK(syn, form, arg);
    if (lispm_obj_is_nil(form)) {
      evframe_leave(old_frame);
      return gc(arg, mark);
    }

    LISPM_ASSERT(lispm_obj_is_builtin_sym(form));
    const struct LispmBuiltin *bi = lispm_builtins_start + lispm_builtin_sym_offs(form);
    LISPM_EVAL_CHECK(bi->eval, form, panic, "a function expected, got: ", form);

    /* do not attempt to gc here, as M.frame objects should never be moved! */
    syn = bi->eval(arg);
  }
}

/* API */
Obj lispm_eval0(const char *pc, const char *pc_end) {
  unsigned mark = M.sp - M.stack;
  return gc(eval(sema(lispm_parse_quote0(pc, pc_end))), mark);
}
static inline int lispm_is_valid_result(Obj e) {
  return lispm_obj_is_nil(e) || lispm_obj_is_atom(e) || lispm_obj_is_cons(e);
}

void lispm_init(void) {
  /* TODO: this function can throw, and it is usually called unguarded, which is
   * wrong */
  LISPM_ASSERT(lispm_is_valid_config());
  M.sp = M.stack_end;
  M.tp = M.strings + 4; /* we use zero as sentinel value for missing entry in htable,
                           hence no value can have a zero offset into the strings table */
  M.htable_index_size = (M.htable_end - M.htable) >> 1;
  M.htable_index_shift = __builtin_clz(M.htable_index_size) + 1;
  M.frame = NIL;
  M.frame_depth = 1;

  const unsigned bilen = lispm_builtins_end - lispm_builtins_start;
  lispm_st_obj_alloc0(bilen);
  for (int i = 0; i < bilen; ++i) {
    const struct LispmBuiltin *bi = lispm_builtins_start + i;
    const char *n = bi->name;
    if (!n) continue;
    Obj s = htable_ensure(n, n + __builtin_strlen(n), 0);
    if (htable_entry_is_lvalue(s)) {
      unsigned *entry = M.htable + lispm_literal_ht_offs(s);
      unsigned not_rvalue = bi->sema ? 1 : 0;
      entry[0] = (entry[0] & ~2u) | not_rvalue;
      entry[1] = LISPM_MAKE_BUILTIN_SYM(i);
    }
    M.stack_end[~i] = s;
  }
}

static void lispm_main(void) {
  M.stack_bottom_mark = lispm_rt_stack_mark();
  Obj r = lispm_eval0(M.pc, M.program_end);
  LISPM_EVAL_CHECK(lispm_is_valid_result(r), r, panic, "invalid result of evaluation: ", r);
  M.stack[0] = r;
}

Obj lispm_exec(void) {
  LISPM_ASSERT(M.htable_index_size != 0); /* check that the machine has been initialized */
  lispm_rt_try(lispm_main);
  return M.stack[0];
}

static Obj PANIC(Obj a) { LISPM_EVAL_CHECK(0, a, panic, "user panic: ", a); }

const struct LispmBuiltin LISPM_SYN[] __attribute__((section(".lispm.rodata.builtins.core"), aligned(16), used)) = {
    {"#err!"},
    {"(apply)", evapply},
    {"(assoc)", evassoc},
    {"quote", evquote, sema_quote},
    {"lambda", evlambda, sema_lambda},
    {"let", evlet, sema_let},
    {"letrec", evletrec, sema_letrec},
    {"cond", evcon, sema_con},
    {"panic!", PANIC},
};
