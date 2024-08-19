#include "lispm.h"
#include "rt.h"
#include "symtable.h"

/* symbol: last two bits encode its kind
 * - <OFFS> 00: symbol from the table, at '<OFFS> 00' position;
 * -  <NUM> 10: for tokens that represent decimal representation of bits of
 * <NUM>;
 * - <CONS> 01: (CONS car cdr), stored at stack CONS (car) and CONS+1 (cdr)
 * position;
 * - <BITS> 11: special marker.
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

/* is atom? */
static inline Sym Atom(Sym x) { return !(x & 1u) ? SYM_T : SYM_NIL; }
/* is literal atom? */
static inline Sym Literal(Sym x) { return (x & 3u) == 0 ? SYM_T : SYM_NIL; }
/* is list? */
static inline Sym List(Sym x) { return (x & 3u) == 1 ? SYM_T : SYM_NIL; }
/* is unsigned? */
static inline Sym Unsigned(Sym x) { return (x & 3u) == 2 ? SYM_T : SYM_NIL; }
/* is special? */
static inline Sym Special(Sym x) { return (x & 3u) == 3 ? SYM_T : SYM_NIL; }

/* ints */
static inline Sym MakeUnsigned(unsigned val) {
  ASSERT(!(val & ~(~0u >> 2)));
  return (val << 2) | 2;
}
static inline unsigned GetUnsigned(Sym val) {
  ASSERT(Unsigned(val));
  return val >> 2;
}

extern const char *STRINGS_TABLE;
static inline const char *LiteralName(Sym x) {
  ASSERT(Literal(x));
  return STRINGS_TABLE + x;
}

/* specials */
#define MAKE_SPECIAL(val) (((val) << 2) | 3u)

/* lisp style Eq */
static inline Sym Eq(Sym x, Sym y) {
  return Atom(x) && x == y ? SYM_T : SYM_NIL;
}

/* hash */
static inline unsigned Djb2(const char *s) {
  unsigned hash = 5381u;
  int c;
  while ((c = *s++))
    hash = 33 * hash + ((unsigned)c);
  return hash;
}

/* Unlike LISPM_ASSERT, these errors are caused by a bug in the user code. */
#define THROW_UNLESS(cond, err)                                                \
  do {                                                                         \
    if (!(cond)) {                                                             \
      sym = (err);                                                             \
      THROW(1);                                                                \
    }                                                                          \
  } while (0)

/* state */
static struct Page *page_table;
#define PAGE_PROGRAM 0u
#define PAGE_STRINGS 1u

static const char *pc; /* program text counter */

Sym *STACK;
static unsigned sp; /* stack pointer, grows down */

static char *STRINGS_TABLE_W;
const char *STRINGS_TABLE;
static unsigned tp; /* table pointer, grows up */

static unsigned *HTABLE;

static Sym sym; /* adjusted by lexer, parser, and eval */

/* list routines */
static inline void InitStack(void) {
  STACK = page_alloc(STACK_SIZE * sizeof(Sym));
  sp = STACK_SIZE;
  THROW_UNLESS(STACK, ERR_OOM);
}

/* list constructor */
static inline Sym Cons(Sym car, Sym cdr) {
  THROW_UNLESS(sp & ~3u, ERR_OOM);
  STACK[--sp] = cdr;
  STACK[--sp] = car;
  return (sp << 2) | 1;
}

static inline Sym Car(Sym a) {
  THROW_UNLESS(List(a), ERR_EVAL);
  return STACK[(a >> 2) + 0];
}
static inline Sym Cdr(Sym a) {
  THROW_UNLESS(List(a), ERR_EVAL);
  return STACK[(a >> 2) + 1];
}
static inline Sym Caar(Sym a) { return Car(Car(a)); }
static inline Sym Cadr(Sym a) { return Car(Cdr(a)); }
static inline Sym Cdar(Sym a) { return Cdr(Car(a)); }
static inline Sym Cddr(Sym a) { return Cdr(Cdr(a)); }

/* string equality: needle is expected to be short(er) */
static inline int StrEq(const char *n, const char *h) {
  while (*n && *n == *h)
    ++n, ++h;
  return *n == *h;
}

static void InitTable(void);
static unsigned *Lookup(const char *lit);
static Sym Insert(const char *lit, unsigned value, int allow_existing);

static const char TABLE_INIT[] = TABLE_PREFIX;
static void InitTable(void) {
  STRINGS_TABLE = STRINGS_TABLE_W = page_alloc(STRINGS_SIZE);
  HTABLE = page_alloc(HTABLE_SIZE * 2 * sizeof(unsigned));
  THROW_UNLESS(STRINGS_TABLE && HTABLE, ERR_OOM);

  tp = 0;
  for (unsigned t = 0; t < sizeof(TABLE_INIT); ++t) {
    if (!TABLE_INIT[t]) continue;
    Sym key = Insert(TABLE_INIT + t, 0, 0);
    ASSERT(key == t);
    while (TABLE_INIT[t++])
      ;
    --t;
  }
  ASSERT(tp + 1 == sizeof(TABLE_INIT));
  Insert("PROGRAM", PAGE_PROGRAM, 0);
  Insert("STRINGS", PAGE_STRINGS, 0);
  page_table[PAGE_STRINGS] = (struct Page){
      .begin = STRINGS_TABLE_W, .end = STRINGS_TABLE_W + STRINGS_SIZE};
}

static unsigned *Lookup(const char *lit) {
  unsigned offset = Djb2(lit);
  const unsigned end_offset = offset + HTABLE_SIZE;
  unsigned *entry;
  LOG("looking for %s starting at %u\n", lit, offset);
  do {
    offset &= (HTABLE_SIZE - 1);
    entry = HTABLE + (2 * offset);
    if (!*entry) {
      /* empty slot */
      return entry;
    }
    if (StrEq(lit, STRINGS_TABLE_W + *entry)) return entry; /* found! */
  } while (++offset != end_offset);
  return 0;
}

Sym Insert(const char *lit, unsigned value, int allow_existing) {
  unsigned *entry = Lookup(lit);
  THROW_UNLESS(entry, ERR_OOM);
  if (allow_existing && *entry) return *entry;

  THROW_UNLESS(!*entry, ERR_EVAL);
  entry[0] = tp;
  entry[1] = value;
  do
    THROW_UNLESS(tp < STRINGS_SIZE, ERR_OOM);
  while ((STRINGS_TABLE_W[tp++] = *lit++));
  tp += 3u;
  tp &= ~3u;
  LOG("inserted symbol %u: %s\n", sym, LiteralName(sym));
  return entry[0];
}

/* lexer */
#define TOKEN_SIZE 128u
static char TOKEN[TOKEN_SIZE];

#define TOK_LPAREN MAKE_SPECIAL('(')
#define TOK_RPAREN MAKE_SPECIAL(')')

static void Lex(void) {
  unsigned c;
  do
    THROW_UNLESS(pc < page_table[PAGE_PROGRAM].end, ERR_LEX);
  while ((c = *pc++) <= ' ');

  switch (c) {
  case '(':
    sym = TOK_LPAREN;
    return;
  case ')':
    sym = TOK_RPAREN;
    return;
  }

  unsigned token_len = 0, token_val = 0;
#define TOKEN_VAL_NONE ((unsigned)-1)

  do {
    /* keep some symbols unavailable to regular tokens: !"#$%& */
    THROW_UNLESS(')' < c && c < 127u && token_len < TOKEN_SIZE, ERR_LEX);
    TOKEN[token_len++] = c;

    if (token_val != TOKEN_VAL_NONE && '0' <= c && c <= '9') {
      /* The maximum value in our format is M = (MAXU / 4) */

      /* 8*val > M  */
      unsigned overflow = token_val > (TOKEN_VAL_NONE >> 5);

      /* no UB: unsigned arithm */
      token_val *= 10;
      token_val += (c - '0');

      /* if overflow was false, then
         no unsigned overflow of token_val can happen,
         as prior value is at least 32 times less */
      overflow |= token_val > (TOKEN_VAL_NONE >> 2);
      THROW_UNLESS(!overflow, ERR_LEX);
    } else {
      /* not an integer literal */
      token_val = TOKEN_VAL_NONE;
    }
    THROW_UNLESS(pc < page_table[PAGE_PROGRAM].end, ERR_LEX);
  } while ((c = *pc++) > ')');
  --pc;

  if (token_val != TOKEN_VAL_NONE) {
    sym = MakeUnsigned(token_val);
    return;
  }

  THROW_UNLESS(token_len < TOKEN_SIZE, ERR_LEX);
  TOKEN[token_len] = 0;
  sym = Insert(TOKEN, 0, 1);
}

/* parser */
#define PARSE_STACK_SIZE 1024u
static Sym PARSE_STACK[PARSE_STACK_SIZE];

static void ParseObject(void) {
  unsigned parse_stack_depth;
  Sym car, li;

  parse_stack_depth = 0;
  do {
    THROW_UNLESS(parse_stack_depth < PARSE_STACK_SIZE, ERR_PARSE);
    Lex();

    switch (sym) {
    case TOK_LPAREN:
      /* make oparen consume 2! slots */
      PARSE_STACK[++parse_stack_depth] = TOK_LPAREN;
      break;
    case TOK_RPAREN:
      li = SYM_NIL;
      do {
        THROW_UNLESS(parse_stack_depth, ERR_PARSE);
        if ((car = PARSE_STACK[--parse_stack_depth]) == TOK_LPAREN) break;
        li = Cons(car, li);
      } while (1);

      /* put result _under_ the position of oparen */
      THROW_UNLESS(parse_stack_depth, ERR_PARSE);
      PARSE_STACK[--parse_stack_depth] = li;
      break;
    default:
      PARSE_STACK[parse_stack_depth] = sym;
    }
  } while ((++parse_stack_depth) & ~1u);
  sym = PARSE_STACK[0];
}

/* eval */
static Sym Eval(Sym e, Sym a);

/* creates a dense copy of object `s`.
 * offset should already be shifted by 2 bits
 */
static Sym Gc(Sym s, Sym mark, unsigned offset) {
  return (List(s) && s < mark)
             ? Cons(Gc(Car(s), mark, offset), Gc(Cdr(s), mark, offset)) + offset
             : s;
}

static Sym Concat(Sym x, Sym y) {
  return x ? Cons(Car(x), Concat(Cdr(x), y)) : y;
}
static Sym Evcon(Sym c, Sym a) {
  return Eval(Caar(c), a) ? Eval(Car(Cdar(c)), a) : Evcon(Cdr(c), a);
}
static Sym Evlis(Sym m, Sym a) {
  return m ? Cons(Eval(Car(m), a), Evlis(Cdr(m), a)) : SYM_NIL;
}

static inline Sym Pair(Sym a, Sym b) {
  return Cons(a, b); /* NOTE: must be in sync with Assoc */
}
static inline Sym Zip(Sym x, Sym y) {
  return x ? Cons(Pair(Car(x), Car(y)), Zip(Cdr(x), Cdr(y))) : SYM_NIL;
}
static Sym Assoc(Sym e, Sym a) {
  while (Caar(a) != e)
    a = Cdr(a);
  return Cdar(a);
}

static Sym Let(Sym e, Sym a) {
  Sym vars = Car(e);
  Sym body = Cadr(e);
  Sym as, var, val;
  while (vars) {
    as = Car(vars);
    vars = Cdr(vars);

    var = Car(as);
    val = Cadr(as);

    /* forbid shadowing core symbols */
    THROW_UNLESS(Atom(var) && (var >= sizeof(TABLE_INIT) - 1), ERR_EVAL);
    a = Cons(Pair(var, Eval(val, a)), a);
  }
  return Eval(body, a);
}

static Sym Eval(Sym e, Sym a) {
  if (Atom(e)) return Assoc(e, a);

  Sym car = Car(e), cdr = Cdr(e);

  unsigned high_mark = sp;
  switch (car) {
  case SYM_QUOTE:
    return Car(cdr);
  case SYM_ATOM:
    e = Atom(Eval(Car(cdr), a));
    break;
  case SYM_CAR:
    e = Car(Eval(Car(cdr), a));
    break;
  case SYM_CDR:
    e = Cdr(Eval(Car(cdr), a));
    break;
  case SYM_EQ:
    e = Eq(Eval(Car(cdr), a), Eval(Cadr(cdr), a));
    break;
  case SYM_CONS:
    e = Cons(Eval(Car(cdr), a), Eval(Cadr(cdr), a));
    break;
  case SYM_COND:
    e = Evcon(cdr, a);
    break;
  case SYM_LET:
    e = Let(cdr, a);
    break;
  default:
    if (Atom(car))
      e = Eval(Cons(Assoc(car, a), cdr), a);
    else if (Car(car) == SYM_LAMBDA)
      e = Eval(Cadr(Cdr(car)), Concat(Zip(Cadr(car), Evlis(cdr, a)), a));
    else
      THROW_UNLESS(0, ERR_EVAL);
  }

  unsigned low_mark = sp;
  e = Gc(e, (high_mark << 2) | 1, (high_mark - low_mark) << 2);
  const unsigned lowest_mark = sp;
  while (lowest_mark < low_mark)
    STACK[--high_mark] = STACK[--low_mark];
  return e;
}

#define TAR_CONTENT_OFFSET 512u
static void lispm_main(struct Page *program) {
  page_table = page_alloc(PAGE_TABLE_SIZE);
  THROW_UNLESS(page_table, ERR_OOM);
  page_table[PAGE_PROGRAM] = *program;
  pc = program->begin + TAR_CONTENT_OFFSET;

  InitStack();
  InitTable();

  ParseObject();

  LOG("parse result:%s\n", "");
  DUMP(sym);
  sym = Eval(sym, SYM_NIL);
}

__attribute__((noreturn)) void lispm_start(struct Page *program) {
  TRY(lispm_main(program));
  DUMP(sym);
  SUCCEED();
}

#if DEBUG_PRINT
static inline void lispm_dump(Sym sym) {
  static int indent = 0;
  static int same_line = 0;

  if (!same_line)
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
  same_line = 0;

  if (Unsigned(sym)) {
    fprintf(stderr, "%u\n", sym);
  } else if (Literal(sym)) {
    fprintf(stderr, "%s\n", LiteralName(sym));
  } else if (Special(sym)) {
    fprintf(stderr, "<special %x>\n", sym);
  } else {
    fprintf(stderr, "(");
    indent += 2;
    same_line = 1;
    while (List(sym)) {
      lispm_dump(STACK[sym >> 2]);
      sym = STACK[(sym >> 2) + 1];
    }
    if (sym) {
      lispm_dump(sym);
    }
    indent -= 2;
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
    fprintf(stderr, sym ? "!)\n" : ")\n");
  }
}
#endif