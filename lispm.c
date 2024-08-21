#include "lispm.h"
#include "rt.h"
#include "sym.h"

#define DEBUG_PRINT 1
#if DEBUG_PRINT
#include <stdio.h>
static void lispm_dump(Sym sym);
#define LOG(fmt, ...) ((void)(fmt))
// #define DUMP(sym) ((void)(sym))
#define DUMP(sym) lispm_dump(sym)
#else
#include "symprint.h"
#include <stdio.h>
#define LOG(fmt, ...) fprintf(stderr, "[%d] " fmt, __LINE__, __VA_ARGS__)
#define DUMP(sym)     lispm_dump(sym)
#endif

static int STATUS;
/* Unlike ASSERT, these errors are caused by a bug in the user code. */
#define THROW_UNLESS(cond, err, ctx)                                           \
  do {                                                                         \
    if (!(cond)) {                                                             \
      STATUS = (err);                                                          \
      if (ctx != 3) DUMP(ctx);                                                 \
      THROW(1);                                                                \
    }                                                                          \
  } while (0)

/* builtin function symbols */
static Sym CONS(Sym a);
static Sym CAR(Sym a);
static Sym CDR(Sym a);
static Sym ATOM(Sym a);
static Sym EQ(Sym a);
struct builtin_fn {
  Sym (*fn)(Sym args);
  const char *name;
};
static const struct builtin_fn BUILTINS[] = {
    {CONS, "CONS"}, {CAR, "CAR"}, {CDR, "CDR"}, {ATOM, "ATOM"}, {EQ, "EQ"}};

/* state */
static struct Page *PAGE_TABLE;
#define PAGE_PROGRAM ((0u << 2) | 3u)

/* program text counter */
static const char *PC;

/* the hash table */
static unsigned *STRINGS_INDEX;
static char *STRINGS_TABLE;
static unsigned TP;

/* the evaluation stack */
static Sym *STACK;
static unsigned SP; /* grows down */
static unsigned PP; /* parser pointer, grows up */

/* stack functions */
static inline void init_stack(void) {
  STACK = page_alloc(STACK_SIZE * sizeof(Sym));
  PP = 8; /* SP may grow below PP by several slots, because cons() and lambda()
             constructors perform check after the fact */
  SP = STACK_SIZE;
  THROW_UNLESS(STACK, STATUS_OOM, 3);
}
static Sym cons(Sym car, Sym cdr) {
  STACK[--SP] = cdr;
  STACK[--SP] = car;
  THROW_UNLESS(PP < SP, STATUS_OOM, 3);
  return make_cons(SP);
}
static inline void cons_unpack(Sym a, Sym *car, Sym *cdr) {
  ASSERT(is_cons(a));
  *car = STACK[cons_st_offs(a) + 0];
  *cdr = STACK[cons_st_offs(a) + 1];
}
static Sym lambda(Sym captures, Sym args, Sym body) {
  STACK[--SP] = body;
  STACK[--SP] = args;
  STACK[--SP] = captures;
  THROW_UNLESS(PP < SP, STATUS_OOM, 3);
  return make_lambda(SP);
}
static void lambda_unpack(Sym a, Sym *captures, Sym *args, Sym *body) {
  ASSERT(is_lambda(a));
  Sym *l = STACK + lambda_st_offs(a);
  *captures = l[0], *args = l[1], *body = l[2];
}
static Sym gc0(Sym s, unsigned high_mark, unsigned depth) {
  if (!is_st_obj(s) || st_obj_st_offs(s) >= high_mark) return s;
  if (is_cons(s)) {
    Sym car, cdr;
    cons_unpack(s, &car, &cdr);
    return st_obj_offset_by(
        cons(gc0(car, high_mark, depth), gc0(cdr, high_mark, depth)), depth);
  }
  ASSERT(is_lambda(s));
  Sym captures, args, body;
  lambda_unpack(s, &captures, &args, &body);
  return st_obj_offset_by(lambda(gc0(captures, high_mark, depth),
                                 gc0(args, high_mark, depth),
                                 gc0(body, high_mark, depth)),
                          depth);
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
static inline Sym get_assoc(Sym s) {
  ASSERT(is_literal(s));
  const Sym a = STRINGS_INDEX[literal_ht_offs(s) + 1];
  THROW_UNLESS(a != SYM_NO_ASSOC, STATUS_EVAL, s);
  return a;
}
static inline Sym set_assoc(Sym s, Sym assoc) {
  Sym *a = STRINGS_INDEX + (literal_ht_offs(s) + 1);
  THROW_UNLESS(!is_special(*a) || !special_is_readonly(*a), STATUS_EVAL, s);
  const Sym old_assoc = *a;
  return *a = assoc, old_assoc;
}
static inline const char *literal_name(Sym s) {
  ASSERT(is_literal(s));
  return STRINGS_TABLE + STRINGS_INDEX[literal_ht_offs(s)];
}

static Sym insert_cstr(const char *lit, Sym assoc);
static void init_table(void) {
  STRINGS_TABLE = page_alloc(STRINGS_SIZE);
  STRINGS_INDEX = page_alloc(STRINGS_INDEX_SIZE * 2 * sizeof(unsigned));
  THROW_UNLESS(STRINGS_TABLE && STRINGS_INDEX, STATUS_OOM, 3);

  TP = 4; /* no symbol starts at offset 0 of the table string */
  insert_cstr("T", SYM_T);
  insert_cstr("QUOTE", SYM_QUOTE);
  insert_cstr("COND", SYM_COND);
  insert_cstr("LAMBDA", SYM_LAMBDA);
  insert_cstr("LET", SYM_LET);
  for (unsigned i = 0; i < sizeof(BUILTINS) / sizeof(*BUILTINS); ++i) {
    insert_cstr(BUILTINS[i].name, MAKE_BUILTIN_FN(i));
  }
}
static Sym ensure(const char *b, const char *e) {
  unsigned offset = djb2(b, e, 5381u);
  const unsigned step = 2 * djb2(b, e, ~offset) + 1; /* must be non-zero */
  int attempt = 0;
  for (; attempt < STRINGS_INDEX_LOOKUP_LIMIT; ++attempt, offset += step) {
    offset &= (STRINGS_INDEX_SIZE - 1);
    unsigned *entry = STRINGS_INDEX + (2 * offset);
    if (!*entry) break; /* empty slot */
    if (str_eq(b, e, STRINGS_TABLE + *entry))
      return make_literal(2 * offset); /* found! */
  }
  THROW_UNLESS(attempt < STRINGS_INDEX_LOOKUP_LIMIT, STATUS_OOM, 3);
  THROW_UNLESS(TP + (e - b + 1) <= STRINGS_SIZE, STATUS_OOM, 3);

  unsigned *entry = STRINGS_INDEX + (2 * offset);
  entry[0] = TP;
  entry[1] = SYM_NO_ASSOC;
  while (b != e)
    STRINGS_TABLE[TP++] = *b++;
  STRINGS_TABLE[TP++] = 0;
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
    THROW_UNLESS(PC < PAGE_TABLE[PAGE_PROGRAM].end, STATUS_LEX, 3);
  while ((c = *PC++) <= ' ');
  if (c == '(' || c == ')') return TOK_SYM(c);

  const char *const token_begin = PC - 1;
  unsigned token_val = 0;
#define TOKEN_VAL_MAX  (~0u >> 2)
#define TOKEN_VAL_NONE ((unsigned)~0u)

  do {
    /* keep some symbols unavailable to regular tokens: !"#$%& */
    THROW_UNLESS(')' < c && c < 127u, STATUS_LEX, 3);
    if (token_val != TOKEN_VAL_NONE && '0' <= c && c <= '9') {
      int overflow =
          __builtin_umul_overflow(token_val, 10u, &token_val) ||
          __builtin_uadd_overflow(token_val, (unsigned)(c - '0'), &token_val) ||
          token_val > TOKEN_VAL_MAX;
      THROW_UNLESS(!overflow, STATUS_LEX, 3);
    } else {
      token_val = TOKEN_VAL_NONE; /* not an integer literal */
    }
    THROW_UNLESS(PC < PAGE_TABLE[PAGE_PROGRAM].end, STATUS_LEX, 3);
  } while ((c = *PC++) > ')');
  --PC;
  return token_val == TOKEN_VAL_NONE ? ensure(token_begin, PC)
                                     : make_unsigned(token_val);
}

/* parser */
static Sym parse_object(Sym tok) {
  if (is_atom(tok)) return tok;
  THROW_UNLESS(tok == TOK_LPAREN, STATUS_PARSE, 3);
  if ((tok = lex()) == TOK_RPAREN) return SYM_NIL;
  unsigned low_mark = PP;
  while (tok != TOK_RPAREN) {
    THROW_UNLESS(++PP < SP, STATUS_PARSE, tok);
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
static inline int is_nil(Sym e) { return e == SYM_NIL; }
static inline int is_t(Sym e) { return e == SYM_T; }
static inline int is_valid_result(Sym e) {
  return is_nil(e) || is_t(e) || is_atom(e) || is_cons(e);
}
static Sym eval(Sym e) {
  e = eval0(e);
  THROW_UNLESS(is_valid_result(e), STATUS_EVAL, e);
  return e;
}
static void cons_unpack_user(Sym a, Sym *car, Sym *cdr) {
  /* cons unpack on user input; soft failure mode */
  THROW_UNLESS(is_cons(a), STATUS_EVAL, a);
  cons_unpack(a, car, cdr);
}
static inline Sym cons_unwrap_last(Sym a) {
  Sym r, n;
  cons_unpack_user(a, &r, &n);
  THROW_UNLESS(is_nil(n), STATUS_EVAL, a);
  return r;
}
static void restore_shadow(Sym s, Sym s0) {
  while (s != s0) {
    Sym a, n, v;
    cons_unpack(s, &a, &s);
    cons_unpack(a, &n, &v);
    set_assoc(n, v);
  }
}
static Sym evcon(Sym bs) {
  Sym b, c, a;
  while (!is_nil(bs)) {
    cons_unpack_user(bs, &b, &bs);
    cons_unpack_user(b, &c, &a);
    if (!is_nil(eval0(c))) return eval0(cons_unwrap_last(a));
  }
  THROW_UNLESS(0, STATUS_EVAL, bs);
}
static Sym evlet(Sym e) {
  Sym c, b, a, n, v;
  cons_unpack_user(e, &c, &b);
  b = cons_unwrap_last(b);
  while (!is_nil(c)) {
    cons_unpack_user(c, &a, &c);
    cons_unpack_user(a, &n, &v);
    set_assoc(n, eval0(cons_unwrap_last(v)));
  }
  return eval0(b);
}
static Sym evlis(Sym e) {
  if (is_nil(e)) return e;
  Sym a, d;
  cons_unpack_user(e, &a, &d);
  return cons(eval0(a), evlis(d));
}
static Sym evapply(Sym f, Sym a) {
  a = evlis(a);
  if (is_literal(f)) f = get_assoc(f);
  if (is_builtin_fn(f)) return BUILTINS[builtin_fn_ft_offs(f)].fn(a);
  if (is_cons(f)) f = eval0(f);
  THROW_UNLESS(is_lambda(f), STATUS_EVAL, f);
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
  THROW_UNLESS(is_nil(a), STATUS_EVAL, a);
  Sym res = eval0(b);
  return restore_shadow(s, SYM_NIL), res;
}
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
    c = evcap0(cons_unwrap_last(e), c);
  }
  return c;
}
static Sym evcap_let(Sym t, Sym c0) {
  Sym a, b, e, e1, n, c = c0;
  cons_unpack_user(t, &a, &e);
  while (!is_nil(a)) {
    cons_unpack_user(a, &b, &a);
    cons_unpack_user(b, &n, &e1);
    c = evcap0(e, c);
    c = cons(cons(n, set_assoc(n, SYM_BINDING)), c);
  }
  c = evcap0(e, c);
  restore_shadow(c, c0);
  return evcap_remove_bindings(c, c0);
}
static Sym evcap0(Sym p, Sym c) {
  if (is_unsigned(p)) return c;
  if (is_literal(p)) {
    const Sym a = get_assoc(p);
    if (is_special(a) && special_is_readonly(a)) return c;
    if (a == SYM_CAPTURED || a == SYM_BINDING) return c;
    return cons(cons(p, set_assoc(p, SYM_CAPTURED)), c);
  }

  Sym a, t, b;
  cons_unpack_user(p, &a, &t);
  if (is_literal(a)) {
    switch (get_assoc(a)) {
    case SYM_QUOTE:
      return cons_unwrap_last(t), c;
    case SYM_COND:
      return evcap_con(t, c);
    case SYM_LET:
      return evcap_let(t, c);
    case SYM_LAMBDA:
      cons_unpack_user(t, &p, &b);
      b = cons_unwrap_last(b);
      return evcap(p, b, c);
    }
  }
  while (!is_nil(p)) {
    cons_unpack_user(p, &a, &p);
    c = evcap0(a, c);
  }
  return c;
}
static Sym evlambda(Sym t) {
  Sym p, b;
  cons_unpack_user(t, &p, &b);
  b = cons_unwrap_last(b);
  return lambda(evcap(p, b, SYM_NIL), p, b);
}
static Sym eval0(Sym e) {
  if (is_unsigned(e)) return e;
  if (is_literal(e)) return get_assoc(e);

  unsigned mark = SP;
  Sym a, t;
  cons_unpack_user(e, &a, &t);
  if (is_literal(a)) {
    switch (get_assoc(a)) {
    case SYM_NIL:
    case SYM_T:
      return a;
    case SYM_QUOTE:
      return cons_unwrap_last(t);
    case SYM_COND:
      return gc(evcon(t), mark);
    case SYM_LET:
      return gc(evlet(t), mark);
    case SYM_LAMBDA:
      return gc(evlambda(t), mark);
    }
  }
  return gc(evapply(a, t), mark);
}

static inline Sym CONS(Sym a) {
  Sym x, y;
  cons_unpack_user(a, &x, &y);
  return cons(x, cons_unwrap_last(y));
}
static inline Sym CAR(Sym a) {
  Sym car, cdr;
  cons_unpack_user(cons_unwrap_last(a), &car, &cdr);
  return car;
}
static inline Sym CDR(Sym a) {
  Sym car, cdr;
  cons_unpack_user(cons_unwrap_last(a), &car, &cdr);
  return cdr;
}
static inline Sym ATOM(Sym a) {
  return is_atom(cons_unwrap_last(a)) ? SYM_T : SYM_NIL;
}
static inline Sym EQ(Sym a) {
  Sym x, y;
  cons_unpack_user(a, &x, &y);
  y = cons_unwrap_last(y);
  return is_atom(x) && x == y ? SYM_T : SYM_NIL;
}

#define TAR_CONTENT_OFFSET 512u
static void lispm_main(struct Page *program) {
  STATUS = STATUS_OK;
  PAGE_TABLE = page_alloc(PAGE_TABLE_SIZE);
  THROW_UNLESS(PAGE_TABLE, STATUS_OOM, 3);
  PAGE_TABLE[PAGE_PROGRAM] = *program;
  PC = program->begin + TAR_CONTENT_OFFSET;

  init_stack();
  init_table();
  Sym res = eval(parse_object(lex()));
  DUMP(res);
}

__attribute__((noreturn)) void lispm_start(struct Page *program) {
  TRY(lispm_main(program));
  if (STATUS == STATUS_EVAL) {
    unsigned tp = 4;
    while (STRINGS_TABLE[tp]) {
      const char *e = STRINGS_TABLE + tp;
      while (*e)
        ++e;
      fprintf(stderr, "%s:\n", STRINGS_TABLE + tp);
      Sym s = ensure(STRINGS_TABLE + tp, e);
      Sym a = STRINGS_INDEX[literal_ht_offs(s) + 1];
      DUMP(a);
      tp = (e - STRINGS_TABLE) + 4;
      tp &= ~3u;
    }
  }
  FIN(STATUS);
}

#if DEBUG_PRINT
static inline void lispm_dump(Sym sym) {
  static int indent = 0;
  static int same_line = 0;

  if (!same_line)
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
  same_line = 0;

  if (is_nil(sym)) {
    fprintf(stderr, "()\n");
  } else if (sym == SYM_T) {
    fprintf(stderr, "T\n");
  } else if (is_unsigned(sym)) {
    fprintf(stderr, "%u\n", sym >> 2);
  } else if (is_literal(sym)) {
    fprintf(stderr, "%s\n", literal_name(sym));
  } else if (is_special(sym)) {
    fprintf(stderr, "<special %u>\n", sym);
  } else if (is_cons(sym)) {
    fprintf(stderr, "(");
    indent += 2;
    same_line = 1;
    while (is_cons(sym)) {
      Sym car;
      cons_unpack(sym, &car, &sym);
      lispm_dump(car);
    }
    if (!is_nil(sym)) {
      lispm_dump(sym);
    }
    indent -= 2;
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
    fprintf(stderr, !is_nil(sym) ? "!)\n" : ")\n");
  } else if (is_lambda(sym)) {
    Sym cap, par, body;
    lambda_unpack(sym, &cap, &par, &body);
    fprintf(stderr, "(LAMBDA\n");
    indent += 2;
    lispm_dump(par);
    lispm_dump(body);
    lispm_dump(cap);
    indent -= 2;
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
    fprintf(stderr, ")\n");
  }
}
#endif
