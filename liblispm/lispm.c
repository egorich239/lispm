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

static Obj list_reverse_inplace(Obj li) {
  /* not a public interface because it actually changes the object state */
  LISPM_ASSERT(lispm_obj_is_nil(li) || lispm_obj_is_st_obj(li));
  Obj cur = li, prev = LISPM_SYM_NIL, next, *cons;
  while (!lispm_obj_is_nil(cur)) {
    cons = lispm_obj_unpack(cur), next = cons[0];
    cons[0] = prev, prev = cur, cur = next;
  }
  return prev;
}

/* stack functions */
static inline Obj *lispm_st_obj_alloc0(unsigned size) {
  LISPM_EVAL_CHECK(M.stack + LISPM_STACK_BOTTOM_OFFSET + size <= M.sp, LISPM_SYM_NIL, oom_stack);
  return M.sp -= size;
}
Obj lispm_obj_alloc0(enum LispmStObjKind k) {
  return lispm_make_st_obj(k, lispm_st_obj_alloc0(lispm_st_obj_st_size(k)) - M.stack);
}
Obj *lispm_obj_unpack(Obj s) { return M.stack + lispm_st_obj_st_offs(s); }
static Obj gc0(Obj s, unsigned high_mark, unsigned depth) {
  /* TODO: guarantee that gc0 over list (of atoms) is non-recursive */
  if (!lispm_obj_is_st_obj(s) || lispm_st_obj_st_offs(s) >= high_mark) return s;
  TRACE_NATIVE_STACK();
  unsigned sz = lispm_st_obj_st_size(s);
  Obj res = lispm_obj_alloc0(lispm_st_obj_kind(s));
  Obj *t = M.sp, *f = lispm_obj_unpack(s) + sz;
  for (Obj *m = t + sz; m != t;)
    *--m = gc0(*--f, high_mark, depth);
  return lispm_st_obj_offset_by(res, depth);
}
static Obj gc(Obj root, unsigned high_mark) {
  unsigned low_mark = M.sp - M.stack;
  root = gc0(root, high_mark, high_mark - low_mark);
  const unsigned lowest_mark = M.sp - M.stack;
  LISPM_TRACE(stack_depth, LISPM_TRACE_STACK_OBJECTS, M.stack_end - M.sp);
  while (lowest_mark < low_mark)
    M.stack[--high_mark] = M.stack[--low_mark];
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
static Obj htable_shadow_append(Obj shadow, Obj lit, Obj assoc) {
  return T(lit, htable_entry_set_assoc(lit, assoc), shadow);
}
static void htable_shadow_rollback(Obj shadow) {
  FOR_EACH_T(name, val, shadow) { M.htable[lispm_literal_ht_offs(name) + 1] = val; }
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
  LISPM_EVAL_CHECK(0, LISPM_SYM_NIL, oom_htable);

ensure_insert:
  LISPM_EVAL_CHECK(M.tp + (e - b + 1) <= M.strings_end, LISPM_SYM_NIL, oom_strings);
  LISPM_EVAL_CHECK(*b != '#' || !is_lex, LISPM_SYM_NIL, lex_error);
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
  LISPM_EVAL_CHECK(0, LISPM_SYM_NIL, lex_error);
lex_atom:
  return htable_ensure(token_begin, M.pc, 1);
lex_num:
  if (token_val != 0 && *token_begin == '0') goto lex_fail;
  return lispm_make_shortnum(token_val);
}
/* parser */
static void parse_frame_enter(void) {
  ++M.lex_frame_depth;
  M.lex_frame = C(LISPM_SYM_NIL, M.lex_frame);
}
static void parse_frame_shadow_append(Obj lit, Obj old_assoc) {
  Obj old_shadow = C_CAR(M.lex_frame);
  C_CAR(M.lex_frame) = T(lit, old_assoc, old_shadow);
}
static void parse_frame_bind(Obj lit, unsigned rec) {
  LISPM_ASSERT(lispm_obj_is_literal(lit));
  if (!htable_entry_is_lvalue(lit)) goto frame_bind_fail;
  Obj new_assoc = PARSE_SYM_BOUND(M.lex_frame_depth, rec), old_assoc = htable_entry_set_assoc(lit, new_assoc);
  if (old_assoc == new_assoc) goto frame_bind_fail;
  return parse_frame_shadow_append(lit, old_assoc);
frame_bind_fail:
  LISPM_EVAL_CHECK(0, lit, illegal_bind, lit);
}
static void parse_frame_use(Obj lit) {
  LISPM_ASSERT(lispm_obj_is_literal(lit));
  if (!htable_entry_is_lvalue(lit)) return;
  Obj old_assoc = htable_entry_get_assoc(lit);
  LISPM_EVAL_CHECK(old_assoc != PARSE_SYM_UNBOUND, lit, unbound_symbol, lit);
  if (old_assoc == PARSE_SYM_BOUND(M.lex_frame_depth, 0) || old_assoc == PARSE_SYM_FREE(M.lex_frame_depth) ||
      (old_assoc & 15u) == 15u)
    return;
  parse_frame_shadow_append(lit, htable_entry_set_assoc(lit, PARSE_SYM_FREE(M.lex_frame_depth)));
}
static Obj parse_frame_leave(void) {
  Obj shadow, bound = PARSE_SYM_BOUND(M.lex_frame_depth--, 0);
  C_UNPACK(M.lex_frame, shadow, M.lex_frame);

  Obj captures = LISPM_SYM_NIL;
  FOR_EACH_T(var, old_assoc, shadow) {
    Obj cur = htable_entry_set_assoc(var, old_assoc);
    if (cur == bound || (cur & 15u) == 15u) continue;
    captures = C(var, captures);
    parse_frame_use(var);
  }
  return captures;
}

static Obj parse(Obj tok) {
  if (lispm_obj_is_atom(tok)) return tok;
  TRACE_NATIVE_STACK();

  if (tok == TOK_QUOTE) return C(M.stack_end[~BUILTIN_QUOTE_INDEX], C(parse(lex()), LISPM_SYM_NIL));
  LISPM_EVAL_CHECK(tok == TOK_LPAREN, tok, parse_error, tok);
  if ((tok = lex()) == TOK_RPAREN) return LISPM_SYM_NIL;

  Obj res = C(parse(tok), LISPM_SYM_NIL);
  while ((tok = lex()) != TOK_RPAREN)
    res = C(parse(tok), res);
  return list_reverse_inplace(res);
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

static Obj sema(Obj syn);
static Obj sema_quote(Obj args) {
  Obj arg;
  LISPM_EVAL_CHECK(lispm_list_scan(&arg, args, 1) == 1, args, panic, "quote expects exactly one argument, got: ", args);
  return arg;
}
static Obj sema_con(Obj brans) {
  Obj res = LISPM_SYM_NIL, conact[2];
  C_ENSURE(brans, "list of conditional actions expected");
  FOR_EACH_C(bran, brans) {
    LISPM_EVAL_CHECK(lispm_list_scan(conact, bran, 2) == 2, bran, panic,
                     "conditional branch must have form (condition action), got: ", bran);
    res = T(sema(conact[0]), sema(conact[1]), res);
  }
  return list_reverse_inplace(res);
}
static Obj sema_lambda(Obj def) {
  Obj argsbody[2];
  LISPM_EVAL_CHECK(lispm_list_scan(argsbody, def, 2) == 2, def, panic,
                   "lambda must provide (lambda (args...) body), got: ", def);
  parse_frame_enter();
  C_ENSURE(argsbody[0], "list of formal arguments expected")
  FOR_EACH_C(arg, argsbody[0]) {
    LISPM_EVAL_CHECK(lispm_obj_is_literal(arg), arg, parse_error, arg);
    parse_frame_bind(arg, 0);
  }
  Obj body = sema(argsbody[1]), captures = parse_frame_leave();
  return T(captures, argsbody[0], body);
}
static Obj sema_let(Obj def) {
  Obj asgnsexpr[2], asgns = LISPM_SYM_NIL, expr, nameval[2];
  LISPM_EVAL_CHECK(lispm_list_scan(asgnsexpr, def, 2) == 2, def, panic,
                   "let must define a list of assignments followed by expression, got: ", def);
  C_ENSURE(asgnsexpr[0], "list of assignments expected")
  FOR_EACH_C(asgn, asgnsexpr[0]) {
    LISPM_EVAL_CHECK(lispm_list_scan(nameval, asgn, 2) == 2 && lispm_obj_is_literal(nameval[0]), asgn, panic,
                     "assignment must have form (name expr), got: ", asgn);
    asgns = T(nameval[0], sema(nameval[1]), asgns);
    parse_frame_enter();
    parse_frame_bind(nameval[0], 0);
  }
  expr = sema(asgnsexpr[1]);
  FOR_EACH_T(ign1, ign2, asgns) { parse_frame_leave(), ((void)ign1), ((void)ign2); }
  return C(list_reverse_inplace(asgns), expr);
}
static Obj sema_letrec(Obj def) {
  Obj asgnsexpr[2], args = LISPM_SYM_NIL, bodies = LISPM_SYM_NIL, exprs = LISPM_SYM_NIL, nameval[2];
  LISPM_EVAL_CHECK(lispm_list_scan(asgnsexpr, def, 2) == 2, def, panic,
                   "letrec must define a list of assignments followed by expression, got: ", def);
  parse_frame_enter();
  C_ENSURE(asgnsexpr[0], "list of assignments expected");
  FOR_EACH_C(asgn, asgnsexpr[0]) {
    LISPM_EVAL_CHECK(lispm_list_scan(nameval, asgn, 2) == 2 && lispm_obj_is_literal(nameval[0]), asgn, panic,
                     "assignment must have form (name expr), got: ", asgn);
    args = C(nameval[0], args);
    bodies = C(nameval[1], bodies);
    parse_frame_bind(nameval[0], 1);
  }
  FOR_EACH_C(body, bodies) { exprs = C(sema(body), exprs); }
  Obj expr = sema(asgnsexpr[1]), captures = parse_frame_leave();
  return C(T(captures, list_reverse_inplace(args), expr), exprs);
}
static Obj sema_apply(Obj syn) {
  Obj res = LISPM_SYM_NIL;
  FOR_EACH_C(arg, syn) { res = C(sema(arg), res); }
  return list_reverse_inplace(res);
}
static Obj sema(Obj syn) {
  if (lispm_obj_is_nil(syn) || lispm_obj_is_shortnum(syn)) return C(LISPM_MAKE_BUILTIN_SYM(BUILTIN_QUOTE_INDEX), syn);
  if (lispm_obj_is_literal(syn)) {
    parse_frame_use(syn);
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
  return C(LISPM_MAKE_BUILTIN_SYM(BUILTIN_APPLY_INDEX), sema_apply(syn));
}

/* eval */
static Obj eval(Obj e);
static Obj evquote(Obj arg) { return arg; }
static Obj evassoc(Obj arg) {
  LISPM_ASSERT(lispm_obj_is_literal(arg));
  LISPM_EVAL_CHECK(htable_entry_is_rvalue(arg), arg, panic, "cannot use symbol as a value: ", arg);
  Obj res = htable_entry_get_assoc(arg);
  LISPM_EVAL_CHECK(res != PARSE_SYM_UNBOUND, arg, unbound_symbol, arg);
  return res;
}
static Obj evlambda(Obj lambda) {
  Obj names, args, body, captures = LISPM_SYM_NIL;
  T_UNPACK(lambda, names, args, body);
  FOR_EACH_C(name, names) {
    Obj assoc = htable_entry_get_assoc(name);
    LISPM_ASSERT(assoc != PARSE_SYM_UNBOUND);
    captures = T(name, assoc, captures);
  }
  return T(captures, args, body);
}
static Obj evcon(Obj brans) {
  FOR_EACH_T(cond, act, brans) {
    if (!lispm_obj_is_nil(eval(cond))) return eval(act);
  }
  LISPM_EVAL_CHECK(0, LISPM_SYM_NIL, panic, "condition branches exhausted", LISPM_SYM_NIL);
}
static Obj evlet(Obj let) {
  Obj asgns, expr, shadow = LISPM_SYM_NIL;
  C_UNPACK(let, asgns, expr);
  FOR_EACH_T(name, expr, asgns) { shadow = htable_shadow_append(shadow, name, eval(expr)); }
  Obj res = eval(expr);
  htable_shadow_rollback(shadow);
  return res;
}
static Obj evlis(Obj li) {
  Obj res = LISPM_SYM_NIL;
  FOR_EACH_C(expr, li) { res = C(eval(expr), res); }
  return list_reverse_inplace(res);
}
static Obj evapply_lambda(Obj fn, Obj args) {
  LISPM_ASSERT(lispm_obj_is_triplet(fn));
  Obj captures, argf, body, shadow = LISPM_SYM_NIL;
  T_UNPACK(fn, captures, argf, body);
  FOR_EACH_T(name, val, captures) { shadow = htable_shadow_append(shadow, name, val); }

  Obj argv = args, name, value;
  while (argf != LISPM_SYM_NIL && argv != LISPM_SYM_NIL) { /* arguments */
    C_UNPACK(argf, name, argf);
    C_UNPACK(argv, value, argv);
    shadow = htable_shadow_append(shadow, name, value);
  }
  LISPM_EVAL_CHECK(lispm_obj_is_nil(argf) && lispm_obj_is_nil(argv), fn, panic,
                   "number of formal arguments does not match the number of "
                   "passed arguments: ",
                   fn);
  Obj res = eval(body);
  htable_shadow_rollback(shadow);
  return res;
}
static Obj evletrec(Obj arg) {
  Obj lambda, exprs, shadow = LISPM_SYM_NIL;
  C_UNPACK(arg, lambda, exprs);
  FOR_EACH_C(name, T_2(lambda)) { shadow = htable_shadow_append(shadow, name, PARSE_SYM_UNBOUND); }
  Obj closure = C(LISPM_MAKE_BUILTIN_SYM(BUILTIN_LAMBDA_INDEX), lambda);
  Obj apply = C(LISPM_MAKE_BUILTIN_SYM(BUILTIN_APPLY_INDEX), C(closure, exprs));
  Obj res = eval(apply);
  htable_shadow_rollback(shadow);
  return res;
}

static Obj evapply(Obj expr) {
  Obj f, args;
  C_UNPACK(evlis(expr), f, args);

  const struct LispmBuiltin *bi = lispm_obj_is_builtin_sym(f) ? lispm_builtins_start + lispm_builtin_sym_offs(f) : 0;
  LISPM_ASSERT(!bi || !bi->sema);
  LISPM_EVAL_CHECK((bi && bi->eval) || lispm_obj_is_triplet(f), f, panic, "a function expected, got: ", f);

  LISPM_TRACE(apply_enter, f, f, args);
  Obj res = bi ? bi->eval(args) : evapply_lambda(f, args);
  LISPM_TRACE(apply_leave);
  return res;
}
static Obj eval(Obj syn) {
  TRACE_NATIVE_STACK();
  Obj form, args;
  C_UNPACK(syn, form, args);

  LISPM_ASSERT(lispm_obj_is_builtin_sym(form));
  const struct LispmBuiltin *bi = lispm_builtins_start + lispm_builtin_sym_offs(form);
  LISPM_ASSERT(bi->eval);

  unsigned mark = M.sp - M.stack;
  Obj res = gc(bi->eval(args), mark);
  return res;
}

/* API */
Obj lispm_eval0(const char *pc, const char *pc_end) { return eval(sema(lispm_parse_quote0(pc, pc_end))); }
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
  M.lex_frame = LISPM_SYM_NIL;
  M.lex_frame_depth = 1;

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
    {"(apply)", evapply, sema_apply},
    {"(assoc)", evassoc},
    {"quote", evquote, sema_quote},
    {"lambda", evlambda, sema_lambda},
    {"cond", evcon, sema_con},
    {"let", evlet, sema_let},
    {"letrec", evletrec, sema_letrec},
    {"panic!", PANIC},
};
