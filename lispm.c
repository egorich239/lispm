#include "lispm.h"
#include "rt.h"
#include "sym.h"

/* state */
static struct PageDesc *PAGE_TABLE;

/* program text counter */
static const char *PROGRAM_END;
static const char *PC;

/* the hash table */
static unsigned *INDEX;
static char *STRINGS;
static unsigned INDEX_SIZE;
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
  while (i + 1 < ERROR_MESSAGE_SIZE && (STRINGS[i++] = *msg++))
    ;
}

/* stack functions */
static Sym st_alloc(unsigned k, Sym **sp) {
  SP -= st_obj_st_size(k);
  EVAL_CHECK(PP < SP, ERR_OOM);
  *sp = STACK + SP;
  return make_st_obj(k, SP);
}
Sym lispm_alloc_cons(Sym car, Sym cdr) {
  Sym *p;
  Sym res = st_alloc(ST_OBJ_CONS, &p);
  return p[0] = car, p[1] = cdr, res;
}
inline void lispm_cons_unpack(Sym a, Sym *car, Sym *cdr) {
  ASSERT(is_cons(a));
  *car = STACK[st_obj_st_offs(a) + 0];
  *cdr = STACK[st_obj_st_offs(a) + 1];
}
static Sym lispm_alloc_lambda(Sym captures, Sym args, Sym body) {
  Sym *p;
  Sym res = st_alloc(ST_OBJ_LAMBDA, &p);
  return p[0] = captures, p[1] = args, p[2] = body, res;
}
static void lambda_unpack(Sym a, Sym *captures, Sym *args, Sym *body) {
  ASSERT(is_lambda(a));
  Sym *l = STACK + st_obj_st_offs(a);
  *captures = l[0], *args = l[1], *body = l[2];
}
Sym lispm_alloc_pointer(Sym page, Sym offs, Sym len) {
  ASSERT(is_page(page) && is_unsigned(offs) && is_unsigned(len));
  Sym *p;
  Sym res = st_alloc(ST_OBJ_POINTER, &p);
  return p[0] = page, p[1] = offs, p[2] = len, res;
}
inline void lispm_pointer_unpack(Sym ptr, Sym *page, Sym *offs, Sym *len) {
  ASSERT(is_pointer(ptr));
  unsigned s = st_obj_st_offs(ptr);
  *page = STACK[s];
  *offs = STACK[s + 1];
  *len = STACK[s + 2];
}
static Sym gc0(Sym s, unsigned high_mark, unsigned depth) {
  if (!is_st_obj(s) || st_obj_st_offs(s) >= high_mark) return s;
  unsigned sz = st_obj_st_size(s);
  Sym *t;
  Sym res = st_obj_offset_by(st_alloc(st_obj_kind(s), &t), depth);
  Sym *f = STACK + st_obj_st_offs(s) + sz;
  for (Sym *m = t + sz; m != t;) {
    *--m = gc0(*--f, high_mark, depth);
  }
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
static inline unsigned djb2(const char *b, const char *e, unsigned hash) {
  while (b != e)
    hash = 33 * hash + ((unsigned)*b++);
  return hash;
}
static inline int str_eq(const char *b, const char *e, const char *h) {
  while (b != e)
    if (*b++ != *h++) return 0;
  return !*h;
}
static inline Sym lispm_literal_is_builtin(Sym s) {
  ASSERT(is_literal(s));
  const Sym a = INDEX[literal_ht_offs(s) + 1];
  return is_special(a) && special_is_readonly(a);
}
static inline Sym get_assoc(Sym s) {
  ASSERT(is_literal(s));
  const Sym a = INDEX[literal_ht_offs(s) + 1];
  EVAL_CHECK(a != SYM_NO_ASSOC, ERR_EVAL);
  return !is_special(a) ? a : (a & ~SPECIAL_READONLY_BIT);
}
static inline Sym set_assoc(Sym s, Sym assoc) {
  Sym *a = INDEX + (literal_ht_offs(s) + 1);
  EVAL_CHECK(!is_special(*a) || !special_is_readonly(*a), ERR_EVAL);
  const Sym old_assoc = *a;
  return *a = assoc, old_assoc;
}
Sym lispm_literal_name_pointer(Sym s) {
  ASSERT(is_literal(s));
  unsigned offs = INDEX[literal_ht_offs(s)];
  unsigned len = __builtin_strlen(STRINGS + offs);
  return lispm_alloc_pointer(PAGE_STRINGS, make_unsigned(offs),
                             make_unsigned(len));
}

static Sym insert_cstr(const char *lit, Sym assoc);
static Sym ensure(const char *b, const char *e) {
  unsigned offset = djb2(b, e, 5381u);
  const unsigned step = 2 * djb2(b, e, ~offset) + 1; /* must be non-zero */
  int attempt = 0;
  for (; attempt < STRINGS_INDEX_LOOKUP_LIMIT; ++attempt, offset += step) {
    offset &= (INDEX_SIZE - 1);
    unsigned *entry = INDEX + (2 * offset);
    if (!*entry) break; /* empty slot */
    if (str_eq(b, e, STRINGS + *entry))
      return make_literal(2 * offset); /* found! */
  }
  EVAL_CHECK(attempt < STRINGS_INDEX_LOOKUP_LIMIT, ERR_OOM);
  EVAL_CHECK(TP + (e - b + 1) <= STRINGS_SIZE, ERR_OOM);

  unsigned *entry = INDEX + (2 * offset);
  entry[0] = TP;
  entry[1] = SYM_NO_ASSOC;
  while (b != e)
    STRINGS[TP++] = *b++;
  STRINGS[TP++] = 0;
  TP += 3u, TP &= ~3u;
  return make_literal(2 * offset);
}
static inline Sym insert_cstr(const char *lit, Sym assoc) {
  const Sym res = ensure(lit, lit + __builtin_strlen(lit));
  return set_assoc(res, assoc), res;
}

/* pages */
inline struct PageDesc *lispm_page_desc(Sym pg) {
  ASSERT(is_page(pg));
  return PAGE_TABLE + page_pt_offs(pg);
}
unsigned lispm_page_size(Sym pg, int elt_size_log2) {
  ASSERT(is_page(pg));
  const struct PageDesc *p = lispm_page_desc(pg);
  return (p->end - p->begin) >> elt_size_log2;
}
void *lispm_page_loc(Sym pg, unsigned offs, unsigned elt_size) {
  ASSERT(is_page(pg) && !(elt_size & (elt_size - 1)));
  const struct PageDesc *p = PAGE_TABLE + page_pt_offs(pg);
  ASSERT(offs * elt_size <= p->end - p->begin);
  return p->begin + offs * elt_size;
}

/* lexer */
/* Special "symbol" values returned by lexer.
   We utilize the same namespace as builtin functions,
   because lexer never produces special symbols. */
#define TOK_SYM(c) MAKE_BUILTIN_FN(((unsigned)(c)))
#define TOK_LPAREN TOK_SYM('(')
#define TOK_RPAREN TOK_SYM(')')

static Sym lex(void) {
  unsigned c;
  do
    EVAL_CHECK(PC < PROGRAM_END, ERR_LEX);
  while ((c = *PC++) <= ' ');
  if (c == '(' || c == ')') return TOK_SYM(c);

  const char *const token_begin = PC - 1;
  unsigned token_val = 0;
#define TOKEN_VAL_MAX  (~0u >> 2)
#define TOKEN_VAL_NONE ((unsigned)~0u)

  do {
    /* keep some symbols unavailable to regular tokens: !"#$%& */
    EVAL_CHECK(')' < c && c < 127u, ERR_LEX);
    if (token_val != TOKEN_VAL_NONE && '0' <= c && c <= '9') {
      int overflow =
          __builtin_umul_overflow(token_val, 10u, &token_val) ||
          __builtin_uadd_overflow(token_val, (unsigned)(c - '0'), &token_val) ||
          token_val > TOKEN_VAL_MAX;
      EVAL_CHECK(!overflow, ERR_LEX);
    } else {
      token_val = TOKEN_VAL_NONE; /* not an integer literal */
    }
    EVAL_CHECK(PC < PROGRAM_END, ERR_LEX);
  } while ((c = *PC++) > ')');
  --PC;
  if (token_val == TOKEN_VAL_NONE) return ensure(token_begin, PC);
  Sym uns = make_unsigned(token_val);
  EVAL_CHECK(token_val == 0 || *token_begin != '0', ERR_LEX);
  return uns;
}

/* parser */
static Sym parse_object(Sym tok) {
  if (is_atom(tok)) return tok;
  EVAL_CHECK(tok == TOK_LPAREN, ERR_PARSE);
  if ((tok = lex()) == TOK_RPAREN) return SYM_NIL;
  unsigned low_mark = PP;
  while (tok != TOK_RPAREN) {
    EVAL_CHECK(++PP < SP, ERR_PARSE);
    STACK[PP - 1] = parse_object(tok);
    tok = lex();
  }
  Sym res = lispm_alloc_cons(STACK[--PP], SYM_NIL);
  while (low_mark < PP)
    res = lispm_alloc_cons(STACK[--PP], res);
  return res;
}

/* eval */
static Sym eval0(Sym e);
static Sym evcap0(Sym p, Sym c);

static inline int is_t(Sym e) { return e == SYM_T; }
static inline int is_valid_result(Sym e) {
  return is_nil(e) || is_t(e) || is_atom(e) || is_cons(e);
}
void lispm_cons_unpack_user(Sym a, Sym *car, Sym *cdr) {
  /* cons unpack on user input; soft failure mode */
  EVAL_CHECK(is_cons(a), ERR_EVAL);
  lispm_cons_unpack(a, car, cdr);
}
Sym lispm_evcap_quote(Sym a, Sym c) { return c; }
inline Sym lispm_evquote(Sym a) {
  Sym r, n;
  lispm_cons_unpack_user(a, &r, &n);
  EVAL_CHECK(is_nil(n), ERR_EVAL);
  return r;
}
void lispm_args_unpack2(Sym a, Sym *f, Sym *s) {
  lispm_cons_unpack_user(a, f, s);
  *s = lispm_evquote(*s);
}
static Sym lispm_evcap_con(Sym bs, Sym c) {
  Sym b, q, e;
  while (!is_nil(bs)) {
    lispm_cons_unpack_user(bs, &b, &bs);
    lispm_cons_unpack_user(b, &q, &e);
    c = evcap0(q, c);
    c = evcap0(lispm_evquote(e), c);
  }
  return c;
}
static Sym lispm_evcon(Sym bs) {
  Sym b, c, a;
  while (!is_nil(bs)) {
    lispm_cons_unpack_user(bs, &b, &bs);
    lispm_cons_unpack_user(b, &c, &a);
    if (!is_nil(eval0(c))) return eval0(lispm_evquote(a));
  }
  EVAL_CHECK(0, ERR_EVAL);
}
static void lispm_restore_shadow(Sym s, Sym s0) {
  while (s != s0) {
    Sym a, n, v;
    lispm_cons_unpack(s, &a, &s);
    lispm_cons_unpack(a, &n, &v);
    set_assoc(n, v);
  }
}
static Sym lispm_evcap_remove_bindings(Sym c, Sym c0) {
  if (c == c0) return c0;
  Sym a, n, v;
  lispm_cons_unpack(c, &a, &c);
  lispm_cons_unpack(a, &n, &v);
  c = lispm_evcap_remove_bindings(c, c0);
  return v == SYM_BINDING ? c : lispm_alloc_cons(a, c);
}
static Sym lispm_evcap_lambda(Sym t, Sym c0) {
  Sym p, b, n, c = c0;
  lispm_args_unpack2(t, &p, &b);
  while (!is_nil(p)) {
    lispm_cons_unpack_user(p, &n, &p);
    c = lispm_alloc_cons(lispm_alloc_cons(n, set_assoc(n, SYM_BINDING)), c);
  }
  c = evcap0(b, c);
  lispm_restore_shadow(c, c0);
  return lispm_evcap_remove_bindings(c, c0);
}
static Sym lispm_evlambda(Sym t) {
  Sym p, b;
  lispm_args_unpack2(t, &p, &b);
  return lispm_alloc_lambda(lispm_evcap_lambda(t, SYM_NIL), p, b);
}
static Sym lispm_evcap_let(Sym t, Sym c0) {
  Sym a, b, e, e1, n, c = c0;
  lispm_cons_unpack_user(t, &a, &e);
  while (!is_nil(a)) {
    lispm_cons_unpack_user(a, &b, &a);
    lispm_cons_unpack_user(b, &n, &e1);
    c = evcap0(lispm_evquote(e1), c);
    c = lispm_alloc_cons(lispm_alloc_cons(n, set_assoc(n, SYM_BINDING)), c);
  }
  c = evcap0(e, c); /* TODO: this suffix is the same for the evcap_lambda! */
  lispm_restore_shadow(c, c0);
  return lispm_evcap_remove_bindings(c, c0);
}
static Sym lispm_evlet(Sym e) {
  Sym c, b, a, n, v;
  lispm_cons_unpack_user(e, &c, &b);
  b = lispm_evquote(b);
  while (!is_nil(c)) {
    lispm_cons_unpack_user(c, &a, &c);
    lispm_cons_unpack_user(a, &n, &v);
    set_assoc(n, eval0(lispm_evquote(v)));
  }
  return eval0(b);
}
static Sym EVAL(Sym e) { /* can be done in lisp, but let's not */
  return eval0(lispm_evquote(e));
}
static struct Builtin BUILTINS[BUILTINS_TABLE_SIZE] = {
    {"QUOTE", lispm_evquote, lispm_evcap_quote},
    {"COND", lispm_evcon, lispm_evcap_con},
    {"LAMBDA", lispm_evlambda, lispm_evcap_lambda},
    {"LET", lispm_evlet, lispm_evcap_let},
    {"EVAL", EVAL},
    {},
};
static unsigned BUILTINS_SIZE;

static Sym evcap0(Sym p, Sym c) {
  if (is_unsigned(p)) return c; /* unsigned: no need to capture */
  if (is_literal(p)) {
    if (lispm_literal_is_builtin(p))
      return c; /* builtins: no need to capture */
    const Sym a = get_assoc(p);
    if (a == SYM_CAPTURED || a == SYM_BINDING) return c; /* already captured */

    /* not captured yet, mark it as such */
    return lispm_alloc_cons(lispm_alloc_cons(p, set_assoc(p, SYM_CAPTURED)), c);
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
  if (is_literal(a) && lispm_literal_is_builtin(a)) {
    b = get_assoc(a);
    if (is_special_form(b)) return BUILTINS[builtin_fn_ft_offs(b)].evcap(t, c);
  }
  while (!is_nil(p)) {
    lispm_cons_unpack_user(p, &a, &p);
    c = evcap0(a, c);
  }
  return c;
}
static Sym evlis(Sym e) {
  if (is_nil(e)) return e;
  Sym a, d;
  lispm_cons_unpack_user(e, &a, &d);
  return lispm_alloc_cons(eval0(a), evlis(d));
}
static Sym evapply(Sym e) {
  Sym f, a;
  lispm_cons_unpack_user(e, &f, &a);
  int eval_args = 0;
  if (is_cons(f)) { /* rewrite nested calls */
    f = eval0(f);
    eval_args = 1;
  }
  if (is_literal(f)) f = get_assoc(f); /* resolve literal */
  if (!is_special_form(f)) eval_args = 1;
  if (eval_args) a = evlis(a);

  if (is_builtin_fn(f)) return BUILTINS[builtin_fn_ft_offs(f)].eval(a);
  EVAL_CHECK(is_lambda(f), ERR_EVAL);
  Sym c, p, b, as, n, v;
  lambda_unpack(f, &c, &p, &b);
  Sym s = SYM_NIL;
  while (!is_nil(c)) { /* captures */
    lispm_cons_unpack(c, &as, &c);
    lispm_cons_unpack(as, &n, &v);
    ASSERT(v != SYM_BINDING);
    s = lispm_alloc_cons(lispm_alloc_cons(n, set_assoc(n, v)), s);
  }
  while (!is_nil(p)) { /* params */
    lispm_cons_unpack_user(p, &n, &p);
    lispm_cons_unpack_user(a, &v, &a);
    s = lispm_alloc_cons(lispm_alloc_cons(n, set_assoc(n, v)), s);
  }
  EVAL_CHECK(is_nil(a), ERR_EVAL);
  Sym res = eval0(b);
  return lispm_restore_shadow(s, SYM_NIL), res;
}
static Sym eval0(Sym e) {
  if (is_unsigned(e)) return e;
  if (is_literal(e)) return get_assoc(e);
  unsigned mark = SP;
  return gc(evapply(e), mark);
}

/* API */
static int lispm_main(struct PageDesc *table, unsigned offs,
                      const struct Builtin *rt) {
  if (!(PAGE_TABLE = table)) return 1;
  for (int i = 0; i < PAGE_TABLE_PRELUDE_SIZE; ++i) {
    if (!PAGE_TABLE[i].begin) return 1;
    PAGE_TABLE[i].flags = PAGE_FLAG_RO;
  }

  PROGRAM_END = table[page_pt_offs(PAGE_PROGRAM)].end;
  PC = table[page_pt_offs(PAGE_PROGRAM)].begin + offs;

  STACK = PAGE_TABLE[page_pt_offs(PAGE_STACK)].begin;
  STACK_END = PAGE_TABLE[page_pt_offs(PAGE_STACK)].end;
  PP = 64;
  SP = STACK_END - STACK;
  if (SP <= PP) return 1;

  STRINGS = PAGE_TABLE[page_pt_offs(PAGE_STRINGS)].begin;
  STRINGS_SIZE = ((char *)PAGE_TABLE[page_pt_offs(PAGE_STRINGS)].end) - STRINGS;
  INDEX = PAGE_TABLE[page_pt_offs(PAGE_INDEX)].begin;
  INDEX_SIZE =
      (((unsigned *)PAGE_TABLE[page_pt_offs(PAGE_INDEX)].end) - INDEX) / 2;
  if (INDEX_SIZE & (INDEX_SIZE - 1)) return 1;

  TP = ERROR_MESSAGE_SIZE;
  insert_cstr("T", SYM_T | SPECIAL_READONLY_BIT);

  while (BUILTINS[BUILTINS_SIZE].name)
    ++BUILTINS_SIZE;
  while (rt && rt->name) {
    if (BUILTINS_SIZE == BUILTINS_TABLE_SIZE) return 1;
    BUILTINS[BUILTINS_SIZE++] = *rt++;
  }
  for (int i = 0; i < BUILTINS_SIZE; ++i) {
    insert_cstr(BUILTINS[i].name,
                MAKE_BUILTIN_FN(i) | SPECIAL_READONLY_BIT |
                    (BUILTINS[i].evcap ? SPECIAL_FORM_BIT : 0));
  }
  STACK[1] = eval0(parse_object(lex()));
  STACK[0] = is_valid_result(STACK[1]) ? SYM_NIL : ERR_EVAL;
  return 0;
}

Sym lispm_exec(struct PageDesc *table, unsigned offs,
               const struct Builtin *rt) {
  int failed_init = 0;
  TRY(failed_init = lispm_main(table, offs, rt));
  return failed_init           ? ERR_INIT  /* init error */
         : STACK[0] != SYM_NIL ? STACK[0]  /* lex/parse/eval error */
                               : STACK[1]; /* regular result */
}
