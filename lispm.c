#include "lispm.h"
#include "rt.h"

static int STATUS;

/* symbol: last two bits encode its kind
 * -      <OFFS> 00: symbol from the table, at '<OFFS> 00' position;
 * -       <NUM> 10: for atoms that are decimal representation of bits of <NUM>;
 * -      <CONS> 01: (CONS car cdr), stored at stack CONS (car) and CONS+1 (cdr)
 *                   position;
 * -      ... 00 11: special forms (NIL, T, ...);
 * -   <OFFS> 10 11: builtin function at the specific offset in the ftable;
 * -   <PAGE> 01 11: pages;
 * - 1 ... 11 11 11: no value marker in hash table.
 */
typedef unsigned Sym;

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
#define DUMP(sym) lispm_dump(sym)
#endif

/* Unlike ASSERT, these errors are caused by a bug in the user code. */
#define THROW_UNLESS(cond, err, ctx)                                           \
  do {                                                                         \
    if (!(cond)) {                                                             \
      STATUS = (err);                                                          \
      if (ctx != 3) DUMP(ctx);                                                 \
      THROW(1);                                                                \
    }                                                                          \
  } while (0)

/* state */
static struct Page *PAGE_TABLE;
#define PAGE_PROGRAM ((0u << 2) | 3u)

/* program text counter */
static const char *PC;

/* the hash table */
static unsigned *STRINGS_INDEX;
static char *STRINGS_TABLE;
static unsigned TP;

/* the execution stack */
static Sym *STACK;
static unsigned SP; /* grows down */
static unsigned PP; /* parser pointer, grows up */

/* common symbols */
#define SPECIAL_ASSOC_READONLY ~(~0u >> 1)

#define SYM_NIL 0
#define SYM_NO_ASSOC ~SPECIAL_ASSOC_READONLY

#define SPECIAL(cat, val) (((val) << 4) | (((cat) & 3u) << 2) | 3u)
#define SPECIAL_FORM(idx) SPECIAL(0, idx)
#define SYM_T SPECIAL_FORM(0)
#define SYM_QUOTE SPECIAL_FORM(1)
#define SYM_COND SPECIAL_FORM(2)
#define SYM_LAMBDA SPECIAL_FORM(3)
#define SYM_LET SPECIAL_FORM(4)

#define BUILTIN_FN(idx) SPECIAL(2, idx)
#define SYM_CONS BUILTIN_FN(0)
#define SYM_CAR BUILTIN_FN(1)
#define SYM_CDR BUILTIN_FN(2)
#define SYM_ATOM BUILTIN_FN(3)
#define SYM_EQ BUILTIN_FN(4)

static inline int is_nil(Sym x) { return x == SYM_NIL; }
static inline int is_atom(Sym x) { return (x & 1u) == 0; }
static inline int is_literal(Sym x) { return (x & 3u) == 0; }
static inline int is_unsigned(Sym x) { return (x & 3u) == 2; }
static inline int is_cons(Sym x) { return (x & 3u) == 1; }
static inline int is_special(Sym x) { return (x & 3u) == 3; }
static inline int is_page(Sym x) { return (x & 15u) == 7; }
static inline int is_builtin(Sym x) { return (x & 15u) == 11; }

/* literals */
static inline Sym create_literal(unsigned *entry) {
  return (entry - STRINGS_INDEX) << 2;
}
static inline Sym get_assoc(Sym s) {
  ASSERT(is_literal(s));
  Sym a = STRINGS_INDEX[(s >> 2) + 1];
  return !is_special(a) ? a : a & ~SPECIAL_ASSOC_READONLY;
}
static inline int is_shadow_allowed(Sym s) {
  Sym a = STRINGS_INDEX[(s >> 2) + 1];
  return !is_nil(s) && (!is_special(a) || !(a & SPECIAL_ASSOC_READONLY));
}
static inline Sym set_assoc(Sym s, Sym assoc) {
  THROW_UNLESS(is_shadow_allowed(s), STATUS_EVAL, s);
  Sym old_assoc = get_assoc(s);
  STRINGS_INDEX[(s >> 2) + 1] = assoc;
  return old_assoc;
}
static inline const char *literal_name(Sym s) {
  ASSERT(is_literal(s));
  return STRINGS_TABLE + STRINGS_INDEX[(s >> 2) + 0];
}

/* unsigned */
static inline Sym create_unsigned(unsigned val) {
  ASSERT(!(val & ~(~0u >> 2)));
  return (val << 2) | 2;
}

/* list */
static inline Sym cons(Sym car, Sym cdr) {
  STACK[--SP] = cdr;
  STACK[--SP] = car;
  THROW_UNLESS(PP < SP, STATUS_OOM, 3);
  return (SP << 2) | 1;
}
static inline Sym car(Sym a) {
  THROW_UNLESS(is_cons(a), STATUS_EVAL, a);
  return STACK[(a >> 2) + 0];
}
static inline Sym cdr(Sym a) {
  THROW_UNLESS(is_cons(a), STATUS_EVAL, a);
  return STACK[(a >> 2) + 1];
}
static inline Sym caar(Sym a) { return car(car(a)); }
static inline Sym cadr(Sym a) { return car(cdr(a)); }
static inline Sym cdar(Sym a) { return cdr(car(a)); }
static inline Sym cddr(Sym a) { return cdr(cdr(a)); }
static inline unsigned cdr_cell(Sym li) {
  ASSERT(is_cons(li));
  return (li >> 2) + 1;
}

/* strings */
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

/* builtins */
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

/* init routines */
static inline void init_stack(void) {
  STACK = page_alloc(STACK_SIZE * sizeof(Sym));
  PP = 2; /* make it a tiny positive number */
  SP = STACK_SIZE;
  THROW_UNLESS(STACK, STATUS_OOM, 3);
}

static void init_table(void);
static Sym ensure(const char *b, const char *e);
static inline Sym insert_cstr(const char *lit, Sym assoc) {
  const char *e = lit;
  while (*e)
    ++e;
  Sym res = ensure(lit, e);
  set_assoc(res, assoc);
  return res;
}

static void init_table(void) {
  STRINGS_TABLE = page_alloc(STRINGS_SIZE);
  STRINGS_INDEX = page_alloc(STRINGS_INDEX_SIZE * 2 * sizeof(unsigned));
  THROW_UNLESS(STRINGS_TABLE && STRINGS_INDEX, STATUS_OOM, 3);

  TP = 4; /* no symbol starts at offset 0 of the table string */
  insert_cstr("T", SYM_T | SPECIAL_ASSOC_READONLY);
  insert_cstr("QUOTE", SYM_QUOTE | SPECIAL_ASSOC_READONLY);
  insert_cstr("COND", SYM_COND | SPECIAL_ASSOC_READONLY);
  insert_cstr("LAMBDA", SYM_LAMBDA | SPECIAL_ASSOC_READONLY);
  insert_cstr("LET", SYM_LET | SPECIAL_ASSOC_READONLY);
  for (unsigned i = 0; i < sizeof(BUILTINS) / sizeof(*BUILTINS); ++i) {
    insert_cstr(BUILTINS[i].name, BUILTIN_FN(i) | SPECIAL_ASSOC_READONLY);
  }
}

static Sym ensure(const char *b, const char *e) {
  unsigned offset = djb2(b, e, 5381u);
  unsigned step = 2 * djb2(b, e, ~offset) + 1;
  for (int i = 0; i < STRINGS_INDEX_LOOKUP_LIMIT; ++i, offset += step) {
    offset &= (STRINGS_INDEX_SIZE - 1);
    unsigned *entry = STRINGS_INDEX + (2 * offset);
    Sym lit = create_literal(entry);
    if (!*entry) {
      THROW_UNLESS(TP + (e - b + 1) <= STRINGS_SIZE, STATUS_OOM, 3);
      entry[0] = TP;
      entry[1] = SYM_NO_ASSOC;
      while (b != e)
        STRINGS_TABLE[TP++] = *b++;
      STRINGS_TABLE[TP++] = 0;
      TP += 3u;
      TP &= ~3u;
      return lit;
    }
    if (str_eq(b, e, literal_name(lit))) return lit; /* found! */
  }
  THROW_UNLESS(0, STATUS_OOM, 3);
}

/* lexer */
/* Special "symbol" values returned by lexer.
   They correspond to page symbols and cannot be produced by a lexeme. */
#define TOK_LPAREN ((((unsigned)'(') << 2) | 3u)
#define TOK_RPAREN ((((unsigned)')') << 2) | 3u)

static Sym lex(void) {
  unsigned c;
  do
    THROW_UNLESS(PC < PAGE_TABLE[PAGE_PROGRAM].end, STATUS_LEX, 3);
  while ((c = *PC++) <= ' ');
  if (c == '(' || c == ')') return (((unsigned)c) << 2) | 3u;

  const char *const token_begin = PC - 1;
  unsigned token_val = 0;
#define TOKEN_VAL_MAX (~0u >> 2)
#define TOKEN_VAL_NONE ((unsigned)-1)

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
      /* not an integer literal */
      token_val = TOKEN_VAL_NONE;
    }
    THROW_UNLESS(PC < PAGE_TABLE[PAGE_PROGRAM].end, STATUS_LEX, 3);
  } while ((c = *PC++) > ')');
  return token_val == TOKEN_VAL_NONE ? ensure(token_begin, --PC)
                                     : create_unsigned(token_val);
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
/* gc: creates a dense copy of object `s`, offset should already be shifted by 2
 * bits */
static Sym gc(Sym s, Sym mark, unsigned offset) {
  return (is_cons(s) && s < mark)
             ? cons(gc(car(s), mark, offset), gc(cdr(s), mark, offset)) + offset
             : s;
}

static Sym eval(Sym e);
static Sym assoc(Sym e) {
  if (is_unsigned(e)) return e;
  Sym r = get_assoc(e);
  THROW_UNLESS(!is_special(r), STATUS_EVAL, e);
  return r;
}
static void restore_shadow(Sym shadow) {
  while (!is_nil(shadow)) {
    set_assoc(caar(shadow), cdar(shadow));
    shadow = cdr(shadow);
  }
}
static Sym evcon(Sym c) {
  return !is_nil(eval(caar(c))) ? eval(car(cdar(c))) : evcon(cdr(c));
}
static Sym evlis(Sym m) {
  return !is_nil(m) ? cons(eval(car(m)), evlis(cdr(m))) : SYM_NIL;
}
static inline Sym pair(Sym a, Sym b) {
  return cons(a, b); /* NOTE: must be in sync with Assoc */
}
static Sym let(Sym e) {
  Sym ass = car(e), body = cadr(e);
  Sym shadow = SYM_NIL;
  while (!is_nil(ass)) {
    Sym lit = caar(ass), ex = cadr(car(ass));
    shadow = cons(pair(lit, set_assoc(lit, eval(ex))), shadow);
    ass = cdr(ass);
  }
  Sym res = eval(body);
  return restore_shadow(shadow), res;
}
static Sym apply(Sym f, Sym a) {
  a = evlis(a);
  if (is_literal(f)) f = get_assoc(f);
  if (is_builtin(f)) return BUILTINS[f >> 4].fn(a);
  THROW_UNLESS(get_assoc(car(f)) == SYM_LAMBDA, STATUS_EVAL, f);
  Sym lits = cadr(f), shadow = SYM_NIL;
  while (!is_nil(lits)) {
    Sym lit = car(lits), ex = car(a);
    shadow = cons(pair(lit, set_assoc(lit, ex)), shadow);
    lits = cdr(lits), a = cdr(a);
  }
  THROW_UNLESS(is_nil(a), STATUS_EVAL, a);
  Sym res = eval(car(cddr(f)));
  return restore_shadow(shadow), res;
}

static Sym eval(Sym e) {
  if (is_atom(e)) return assoc(e);
  unsigned high_mark = SP;
  if (is_literal(car(e))) {
    switch (get_assoc(car(e))) {
    case SYM_NIL:
    case SYM_T:
      return car(e);
    case SYM_QUOTE:
      return cadr(e);
    case SYM_COND:
      e = evcon(cdr(e));
      break;
    case SYM_LET:
      e = let(cdr(e));
      break;
    default:
      e = apply(car(e), cdr(e));
    }
  } else {
    e = apply(car(e), cdr(e));
  }
  unsigned low_mark = SP;
  e = gc(e, (high_mark << 2) | 1, (high_mark - low_mark) << 2);
  const unsigned lowest_mark = SP;
  while (lowest_mark < low_mark)
    STACK[--high_mark] = STACK[--low_mark];
  return e;
}

static inline Sym CONS(Sym a) {
  Sym x = car(a), y = cadr(a), n = cddr(a);
  THROW_UNLESS(is_nil(n), STATUS_EVAL, a);
  return cons(x, y);
}
static inline Sym CAR(Sym a) {
  Sym x = car(a), n = cdr(a);
  THROW_UNLESS(is_nil(n), STATUS_EVAL, a);
  return car(x);
}
static inline Sym CDR(Sym a) {
  Sym x = car(a), n = cdr(a);
  THROW_UNLESS(is_nil(n), STATUS_EVAL, a);
  return cdr(x);
}
static inline Sym ATOM(Sym a) {
  Sym x = car(a), n = cdr(a);
  THROW_UNLESS(is_nil(n), STATUS_EVAL, a);
  return is_atom(x) ? SYM_T : SYM_NIL;
}
static inline Sym EQ(Sym a) {
  Sym x = car(a), y = cadr(a), n = cddr(a);
  THROW_UNLESS(is_nil(n), STATUS_EVAL, a);
  return is_atom(x) && x == y ? SYM_T : SYM_NIL;
}

#define TAR_CONTENT_OFFSET 512u
static void lispm_main(struct Page *program) {
  PAGE_TABLE = page_alloc(PAGE_TABLE_SIZE);
  THROW_UNLESS(PAGE_TABLE, STATUS_OOM, 3);
  PAGE_TABLE[PAGE_PROGRAM] = *program;
  PC = program->begin + TAR_CONTENT_OFFSET;

  init_stack();
  init_table();

  Sym sym = parse_object(lex());
  DUMP(sym);
  sym = eval(sym);
  DUMP(sym);
}

__attribute__((noreturn)) void lispm_start(struct Page *program) {
  TRY(lispm_main(program));
  FIN(STATUS_OK);
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
  } else if (is_unsigned(sym)) {
    fprintf(stderr, "%u\n", sym >> 2);
  } else if (is_literal(sym)) {
    fprintf(stderr, "%s\n", literal_name(sym));
  } else if (is_page(sym)) {
    fprintf(stderr, "<page %u>\n", (sym >> 2));
  } else {
    fprintf(stderr, "(");
    indent += 2;
    same_line = 1;
    while (is_cons(sym)) {
      lispm_dump(car(sym));
      sym = cdr(sym);
    }
    if (!is_nil(sym)) {
      lispm_dump(sym);
    }
    indent -= 2;
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
    fprintf(stderr, !is_nil(sym) ? "!)\n" : ")\n");
  }
}
#endif
