#include "lispm.h"
#include "rt.h"
#include "sym.h"

/* Unlike ASSERT, these errors are caused by a bug in the user code. */
#define EVAL_CHECK(cond, err, ctx)                                             \
  do {                                                                         \
    if (!(cond)) report_error(err, ctx);                                       \
  } while (0)

/* builtins: functions and special forms */
struct builtin_fn {
  Sym (*fn)(Sym args);
  const char *name;
  unsigned flags;
};

/* state */
static struct Page *PAGE_TABLE;

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
__attribute__((noreturn)) static void report_error(Sym err, Sym ctx) {
  ASSERT(STACK);
  STACK[0] = err;
  STACK[1] = ctx;
  THROW(1);
}

/* stack functions */
static Sym st_alloc(unsigned k, Sym **sp) {
  SP -= st_obj_st_size(k);
  EVAL_CHECK(PP < SP, ERR_OOM, SYM_NIL);
  *sp = STACK + SP;
  return make_st_obj(k, SP);
}
static Sym cons(Sym car, Sym cdr) {
  Sym *p;
  Sym res = st_alloc(ST_OBJ_CONS, &p);
  return p[0] = car, p[1] = cdr, res;
}
static inline void cons_unpack(Sym a, Sym *car, Sym *cdr) {
  ASSERT(is_cons(a));
  *car = STACK[st_obj_st_offs(a) + 0];
  *cdr = STACK[st_obj_st_offs(a) + 1];
}
static Sym lambda(Sym captures, Sym args, Sym body) {
  Sym *p;
  Sym res = st_alloc(ST_OBJ_LAMBDA, &p);
  return p[0] = captures, p[1] = args, p[2] = body, res;
}
static void lambda_unpack(Sym a, Sym *captures, Sym *args, Sym *body) {
  ASSERT(is_lambda(a));
  Sym *l = STACK + st_obj_st_offs(a);
  *captures = l[0], *args = l[1], *body = l[2];
}
static Sym pointer(Sym page, Sym offs, Sym len) {
  ASSERT(is_page(page) && is_unsigned(offs) &&
         (is_nil(len) || is_unsigned(len)));
  Sym *p;
  Sym res = st_alloc(ST_OBJ_POINTER, &p);
  return p[0] = page, p[1] = offs, p[2] = len, res;
}
static inline void pointer_unpack(Sym ptr, const struct Page **page, void **loc,
                                  Sym *len) {
  ASSERT(is_pointer(ptr));
  unsigned s = st_obj_st_offs(ptr);
  *page = PAGE_TABLE + page_pt_offs(STACK[s]);
  *loc = (*page)->begin + STACK[s + 1];
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
static inline Sym has_readonly_assoc(Sym s) {
  ASSERT(is_literal(s));
  const Sym a = INDEX[literal_ht_offs(s) + 1];
  return is_special(a) && special_is_readonly(a);
}
static inline Sym get_assoc(Sym s) {
  ASSERT(is_literal(s));
  const Sym a = INDEX[literal_ht_offs(s) + 1];
  EVAL_CHECK(a != SYM_NO_ASSOC, ERR_EVAL, s);
  return !is_special(a) ? a : (a & ~SPECIAL_READONLY_BIT);
}
static inline Sym set_assoc(Sym s, Sym assoc) {
  Sym *a = INDEX + (literal_ht_offs(s) + 1);
  EVAL_CHECK(!is_special(*a) || !special_is_readonly(*a), ERR_EVAL, s);
  const Sym old_assoc = *a;
  return *a = assoc, old_assoc;
}
static inline const char *literal_name(Sym s) {
  ASSERT(is_literal(s));
  return STRINGS + INDEX[literal_ht_offs(s)];
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
  EVAL_CHECK(attempt < STRINGS_INDEX_LOOKUP_LIMIT, ERR_OOM, SYM_NIL);
  EVAL_CHECK(TP + (e - b + 1) <= STRINGS_SIZE, ERR_OOM, SYM_NIL);

  unsigned *entry = INDEX + (2 * offset);
  entry[0] = TP;
  entry[1] = SYM_NO_ASSOC;
  while (b != e)
    STRINGS[TP++] = *b++;
  STRINGS[TP++] = 0;
  TP += 3u, TP &= ~3u;
  return make_literal(2 * offset);
}
static Sym insert_cstr(const char *lit, Sym assoc) {
  const char *e = lit;
  while (*e)
    ++e;
  const Sym res = ensure(lit, e);
  return set_assoc(res, assoc), res;
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
    EVAL_CHECK(PC < PROGRAM_END, ERR_LEX, SYM_NIL);
  while ((c = *PC++) <= ' ');
  if (c == '(' || c == ')') return TOK_SYM(c);

  const char *const token_begin = PC - 1;
  unsigned token_val = 0;
#define TOKEN_VAL_MAX  (~0u >> 2)
#define TOKEN_VAL_NONE ((unsigned)~0u)

  do {
    /* keep some symbols unavailable to regular tokens: !"#$%& */
    EVAL_CHECK(')' < c && c < 127u, ERR_LEX, SYM_NIL);
    if (token_val != TOKEN_VAL_NONE && '0' <= c && c <= '9') {
      int overflow =
          __builtin_umul_overflow(token_val, 10u, &token_val) ||
          __builtin_uadd_overflow(token_val, (unsigned)(c - '0'), &token_val) ||
          token_val > TOKEN_VAL_MAX;
      EVAL_CHECK(!overflow, ERR_LEX, SYM_NIL);
    } else {
      token_val = TOKEN_VAL_NONE; /* not an integer literal */
    }
    EVAL_CHECK(PC < PROGRAM_END, ERR_LEX, SYM_NIL);
  } while ((c = *PC++) > ')');
  --PC;
  if (token_val == TOKEN_VAL_NONE) return ensure(token_begin, PC);
  Sym uns = make_unsigned(token_val);
  EVAL_CHECK(token_val == 0 || *token_begin != '0', ERR_LEX, uns);
  return uns;
}

/* parser */
static Sym parse_object(Sym tok) {
  if (is_atom(tok)) return tok;
  EVAL_CHECK(tok == TOK_LPAREN, ERR_PARSE, SYM_NIL);
  if ((tok = lex()) == TOK_RPAREN) return SYM_NIL;
  unsigned low_mark = PP;
  while (tok != TOK_RPAREN) {
    EVAL_CHECK(++PP < SP, ERR_PARSE, tok);
    STACK[PP - 1] = parse_object(tok);
    tok = lex();
  }
  Sym res = cons(STACK[--PP], SYM_NIL);
  while (low_mark < PP)
    res = cons(STACK[--PP], res);
  return res;
}

/* eval */
static Sym eval0(Sym e);
static inline int is_t(Sym e) { return e == SYM_T; }
static inline int is_valid_result(Sym e) {
  return is_nil(e) || is_t(e) || is_atom(e) || is_cons(e);
}
static void cons_unpack_user(Sym a, Sym *car, Sym *cdr) {
  /* cons unpack on user input; soft failure mode */
  EVAL_CHECK(is_cons(a), ERR_EVAL, a);
  cons_unpack(a, car, cdr);
}
static void restore_shadow(Sym s, Sym s0) {
  while (s != s0) {
    Sym a, n, v;
    cons_unpack(s, &a, &s);
    cons_unpack(a, &n, &v);
    set_assoc(n, v);
  }
}
static inline Sym evquote(Sym a) {
  Sym r, n;
  cons_unpack_user(a, &r, &n);
  EVAL_CHECK(is_nil(n), ERR_EVAL, a);
  return r;
}
static Sym evcon(Sym bs) {
  Sym b, c, a;
  while (!is_nil(bs)) {
    cons_unpack_user(bs, &b, &bs);
    cons_unpack_user(b, &c, &a);
    if (!is_nil(eval0(c))) return eval0(evquote(a));
  }
  EVAL_CHECK(0, ERR_EVAL, bs);
}
static Sym evcap(Sym p, Sym b, Sym c0);
static Sym evlambda(Sym t) {
  Sym p, b;
  cons_unpack_user(t, &p, &b);
  b = evquote(b);
  return lambda(evcap(p, b, SYM_NIL), p, b);
}
static Sym evlet(Sym e) {
  Sym c, b, a, n, v;
  cons_unpack_user(e, &c, &b);
  b = evquote(b);
  while (!is_nil(c)) {
    cons_unpack_user(c, &a, &c);
    cons_unpack_user(a, &n, &v);
    set_assoc(n, eval0(evquote(v)));
  }
  return eval0(b);
}
static Sym CONS(Sym a) {
  Sym x, y;
  cons_unpack_user(a, &x, &y);
  return cons(x, evquote(y));
}
static Sym CAR(Sym a) {
  Sym car, cdr;
  cons_unpack_user(evquote(a), &car, &cdr);
  return car;
}
static Sym CDR(Sym a) {
  Sym car, cdr;
  cons_unpack_user(evquote(a), &car, &cdr);
  return cdr;
}
static Sym ATOM(Sym a) { return is_atom(evquote(a)) ? SYM_T : SYM_NIL; }
static Sym EQ(Sym a) {
  Sym x, y;
  cons_unpack_user(a, &x, &y);
  return x == evquote(y) && is_atom(x) ? SYM_T : SYM_NIL;
}
static Sym EVAL(Sym e) { /* can be done in lisp, but let's not */
  return eval0(evquote(e));
}
static Sym STR(Sym e) {
  EVAL_CHECK(!is_nil(e), ERR_EVAL, SYM_NIL);
  Sym c;
  unsigned stp = TP;
  while (!is_nil(e)) {
    cons_unpack_user(e, &c, &e);
    if (is_literal(c)) {
      const char *s = literal_name(c);
      int l = __builtin_strlen(s);
      EVAL_CHECK(stp + l + 1 <= STRINGS_SIZE, ERR_OOM, c);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wrestrict"
      /* TODO: something is fishy here */
      __builtin_strcpy(STRINGS + stp, s);
#pragma GCC diagnostic pop
      stp += l;
      continue;
    }
    EVAL_CHECK(is_unsigned(c) && unsigned_val(c) <= 255, ERR_EVAL, c);
    STRINGS[stp++] = unsigned_val(c);
    EVAL_CHECK(stp < STRINGS_SIZE, ERR_OOM, c);
  }
  Sym len = make_unsigned(stp - TP);
  return pointer(
      PAGE_STRINGS,
      make_unsigned(literal_name(ensure(STRINGS + TP, STRINGS + stp)) -
                    STRINGS),
      len);
}
static Sym GETC(Sym a) {
  const struct Page *page;
  void *l;
  Sym ptr = evquote(a), len;
  EVAL_CHECK(is_pointer(ptr), ERR_EVAL, ptr);
  pointer_unpack(ptr, &page, &l, &len);
  return make_unsigned(*(unsigned char *)l);
}
static Sym WRITE(Sym a) {
  Sym dest, src, dest_len, src_len;
  const struct Page *dest_page, *src_page;
  void *dest_loc, *src_loc;
  cons_unpack_user(a, &dest, &src);
  src = evquote(src);
  EVAL_CHECK(is_pointer(dest) && is_pointer(src), ERR_EVAL, a);
  pointer_unpack(dest, &dest_page, &dest_loc, &dest_len);
  pointer_unpack(src, &src_page, &src_loc, &src_len);
  EVAL_CHECK(dest_page->flags == PAGE_FLAG_RW, ERR_EVAL, a);
  EVAL_CHECK(!is_nil(src_len), ERR_EVAL, src_len);
  if (!is_nil(dest_len) && dest_len < src_len) src_len = dest_len;
  __builtin_memcpy(dest_loc, src_loc, unsigned_val(src_len));
  return src_len;
}
static const struct builtin_fn BUILTINS[] = {
    {evquote, "QUOTE", SPECIAL_FORM_BIT},
    {evcon, "COND", SPECIAL_FORM_BIT},
    {evlambda, "LAMBDA", SPECIAL_FORM_BIT},
    {evlet, "LET", SPECIAL_FORM_BIT},
    {STR, "STR", SPECIAL_FORM_BIT},
    {CONS, "CONS"},
    {CAR, "CAR"},
    {CDR, "CDR"},
    {ATOM, "ATOM"},
    {EQ, "EQ"},
    {EVAL, "EVAL"},
    {GETC, "GETC"},
    {WRITE, "WRITE"}};
#define SYM_QUOTE  (MAKE_BUILTIN_FN(0) | SPECIAL_FORM_BIT)
#define SYM_COND   (MAKE_BUILTIN_FN(1) | SPECIAL_FORM_BIT)
#define SYM_LAMBDA (MAKE_BUILTIN_FN(2) | SPECIAL_FORM_BIT)
#define SYM_LET    (MAKE_BUILTIN_FN(3) | SPECIAL_FORM_BIT)
#define SYM_STR    (MAKE_BUILTIN_FN(4) | SPECIAL_FORM_BIT)

static Sym evcap0(Sym p, Sym c);
static Sym evcap_remove_bindings(Sym c, Sym c0) {
  if (c == c0) return c0;
  Sym a, n, v;
  cons_unpack(c, &a, &c);
  cons_unpack(a, &n, &v);
  c = evcap_remove_bindings(c, c0);
  return v == SYM_BINDING ? c : cons(a, c);
}
static Sym evcap(Sym p, Sym b, Sym c0) {
  Sym n, c = c0;
  while (!is_nil(p)) {
    cons_unpack_user(p, &n, &p);
    c = cons(cons(n, set_assoc(n, SYM_BINDING)), c);
  }
  c = evcap0(b, c);
  restore_shadow(c, c0);
  return evcap_remove_bindings(c, c0);
}
static Sym evcap_con(Sym bs, Sym c) {
  Sym b, q, e;
  while (!is_nil(bs)) {
    cons_unpack_user(bs, &b, &bs);
    cons_unpack_user(b, &q, &e);
    c = evcap0(q, c);
    c = evcap0(evquote(e), c);
  }
  return c;
}
static Sym evcap_let(Sym t, Sym c0) {
  Sym a, b, e, e1, n, c = c0;
  cons_unpack_user(t, &a, &e);
  while (!is_nil(a)) {
    cons_unpack_user(a, &b, &a);
    cons_unpack_user(b, &n, &e1);
    c = evcap0(evquote(e1), c);
    c = cons(cons(n, set_assoc(n, SYM_BINDING)), c);
  }
  c = evcap0(e, c);
  restore_shadow(c, c0);
  return evcap_remove_bindings(c, c0);
}
static Sym evcap0(Sym p, Sym c) {
  if (is_unsigned(p)) return c;
  if (is_literal(p)) {
    if (has_readonly_assoc(p)) return c;
    const Sym a = get_assoc(p);
    if (a == SYM_CAPTURED || a == SYM_BINDING) return c;
    return cons(cons(p, set_assoc(p, SYM_CAPTURED)), c);
  }

  Sym a, t, b;
  cons_unpack_user(p, &a, &t);
  if (is_literal(a)) {
    switch (get_assoc(a)) {
    case SYM_STR:
      return c;
    case SYM_QUOTE:
      return evquote(t), c;
    case SYM_COND:
      return evcap_con(t, c);
    case SYM_LET:
      return evcap_let(t, c);
    case SYM_LAMBDA:
      cons_unpack_user(t, &p, &b);
      b = evquote(b);
      return evcap(p, b, c);
    }
  }
  while (!is_nil(p)) {
    cons_unpack_user(p, &a, &p);
    c = evcap0(a, c);
  }
  return c;
}
static Sym evlis(Sym e) {
  if (is_nil(e)) return e;
  Sym a, d;
  cons_unpack_user(e, &a, &d);
  return cons(eval0(a), evlis(d));
}
static Sym evapply(Sym e) {
  Sym f, a;
  cons_unpack_user(e, &f, &a);

  f = eval0(f);
  if (is_special_form(f)) return BUILTINS[builtin_fn_ft_offs(f)].fn(a);
  a = evlis(a);
  if (is_builtin_fn(f)) return BUILTINS[builtin_fn_ft_offs(f)].fn(a);
  EVAL_CHECK(is_lambda(f), ERR_EVAL, f);
  Sym c, p, b, as, n, v;
  lambda_unpack(f, &c, &p, &b);
  Sym s = SYM_NIL;
  while (!is_nil(c)) { /* captures */
    cons_unpack(c, &as, &c);
    cons_unpack(as, &n, &v);
    ASSERT(v != SYM_BINDING);
    s = cons(cons(n, set_assoc(n, v)), s);
  }
  while (!is_nil(p)) { /* params */
    cons_unpack_user(p, &n, &p);
    cons_unpack_user(a, &v, &a);
    s = cons(cons(n, set_assoc(n, v)), s);
  }
  EVAL_CHECK(is_nil(a), ERR_EVAL, a);
  Sym res = eval0(b);
  return restore_shadow(s, SYM_NIL), res;
}
static Sym eval0(Sym e) {
  if (is_unsigned(e)) return e;
  if (is_literal(e)) return get_assoc(e);

  unsigned mark = SP;
  return gc(evapply(e), mark);
}

/* API */
static int lispm_main(struct Page *table, unsigned offs) {
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

  TP = 4; /* no symbol starts at offset 0 of the table string */
  insert_cstr("T", SYM_T | SPECIAL_READONLY_BIT);
  for (int i = 0; i < sizeof(BUILTINS) / sizeof(*BUILTINS); ++i) {
    insert_cstr(BUILTINS[i].name,
                MAKE_BUILTIN_FN(i) | BUILTINS[i].flags | SPECIAL_READONLY_BIT);
  }
  STACK[1] = eval0(parse_object(lex()));
  STACK[0] = is_valid_result(STACK[1]) ? SYM_NIL : ERR_EVAL;
  return 0;
}

Sym lispm_exec(struct Page *table, unsigned offs) {
  int failed_init = 0;
  TRY(failed_init = lispm_main(table, offs));
  return failed_init           ? ERR_INIT  /* init error */
         : STACK[0] != SYM_NIL ? STACK[0]  /* lex/parse/eval error */
                               : STACK[1]; /* regular result */
}
