#include "lispm.h"
#include "rt.h"

static int STATUS;

/* symbol: last two bits encode its kind
 * - <OFFS> 00: symbol from the table, at '<OFFS> 00' position;
 * -  <NUM> 10: for tokens that represent decimal representation of bits of
 * <NUM>;
 * - <CONS> 01: (CONS car cdr), stored at stack CONS (car) and CONS+1 (cdr)
 * position;
 * - <PAGE> 11: page defined at <PAGE> offset of the page table;
 * - 11..11 11: no value marker in hash table.
 */
typedef unsigned Sym;
#define SYM_SHADOW_FORBIDDEN ((~0u) - 4u)
#define SYM_NO_ASSOC (~0u)

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
Sym SYM_NIL, SYM_QUOTE, SYM_COND, SYM_LET, SYM_LAMBDA, SYM_T, SYM_EQ, SYM_ATOM,
    SYM_CAR, SYM_CDR, SYM_CONS, SYM_STR, SYM_PROGRAM;

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

static inline int IsAtom(Sym x) { return !(x & 1u); }
static inline int IsNil(Sym x) { return x == SYM_NIL; }
static inline int IsLiteral(Sym x) { return (x & 3u) == 0; }
static inline int IsUnsigned(Sym x) { return (x & 3u) == 2; }
static inline int IsCons(Sym x) { return (x & 3u) == 1; }
static inline int IsPage(Sym x) { return (x & 3u) == 3; }

static inline Sym MakeLiteral(unsigned *entry) {
  return (entry - STRINGS_INDEX) << 2;
}
static inline const char *LiteralName(Sym s) {
  ASSERT(IsLiteral(s));
  return STRINGS_TABLE + STRINGS_INDEX[(s >> 2) + 0];
}
static inline unsigned *LiteralAssoc(Sym s) {
  ASSERT(IsLiteral(s));
  return STRINGS_INDEX + (s >> 2) + 1;
}
static inline Sym MakeUnsigned(unsigned val) {
  ASSERT(!(val & ~(~0u >> 2)));
  return (val << 2) | 2;
}

/* strings */
static inline unsigned Djb2(const char *b, const char *e, unsigned hash) {
  while (b != e)
    hash = 33 * hash + ((unsigned)*b++);
  return hash;
}

static inline int StrEq(const char *b, const char *e, const char *h) {
  while (b != e)
    if (*b++ != *h++) return 0;
  return !*h;
}

/* init routines */
static inline void InitStack(void) {
  STACK = page_alloc(STACK_SIZE * sizeof(Sym));
  PP = 2; /* make it a tiny positive number*/
  SP = STACK_SIZE;
  THROW_UNLESS(STACK, STATUS_OOM, 3);
}

static void InitTable(void);
static unsigned *Lookup(const char *b, const char *e);
static Sym Insert(const char *b, const char *e, unsigned value,
                  int allow_existing);
static inline Sym InsertCStr(const char *lit, unsigned value,
                             int allow_existing) {
  const char *e = lit;
  while (*e)
    ++e;
  return Insert(lit, e, value, allow_existing);
}

static void InitTable(void) {
  STRINGS_TABLE = page_alloc(STRINGS_SIZE);
  STRINGS_INDEX = page_alloc(STRINGS_INDEX_SIZE * 2 * sizeof(unsigned));
  THROW_UNLESS(STRINGS_TABLE && STRINGS_INDEX, STATUS_OOM, 3);

  TP = 0;
  SYM_NIL = InsertCStr("NIL", SYM_SHADOW_FORBIDDEN, 0);
  SYM_QUOTE = InsertCStr("QUOTE", SYM_SHADOW_FORBIDDEN, 0);
  SYM_COND = InsertCStr("COND", SYM_SHADOW_FORBIDDEN, 0);
  SYM_LET = InsertCStr("LET", SYM_SHADOW_FORBIDDEN, 0);
  SYM_LAMBDA = InsertCStr("LAMBDA", SYM_SHADOW_FORBIDDEN, 0);
  SYM_T = InsertCStr("T", SYM_SHADOW_FORBIDDEN, 0);
  SYM_EQ = InsertCStr("EQ", SYM_SHADOW_FORBIDDEN, 0);
  SYM_ATOM = InsertCStr("ATOM", SYM_SHADOW_FORBIDDEN, 0);
  SYM_CAR = InsertCStr("CAR", SYM_SHADOW_FORBIDDEN, 0);
  SYM_CDR = InsertCStr("CDR", SYM_SHADOW_FORBIDDEN, 0);
  SYM_CONS = InsertCStr("CONS", SYM_SHADOW_FORBIDDEN, 0);
  SYM_STR = InsertCStr("STR", SYM_SHADOW_FORBIDDEN, 0);
  SYM_PROGRAM = InsertCStr("PROGRAM", PAGE_PROGRAM, 0);
}

static unsigned *Lookup(const char *b, const char *e) {
  unsigned offset = Djb2(b, e, 5381u);
  unsigned step = 2 * Djb2(b, e, ~offset) + 1;
  for (int i = 0; i < STRINGS_INDEX_LOOKUP_LIMIT; ++i, offset += step) {
    offset &= (STRINGS_INDEX_SIZE - 1);
    unsigned *entry = STRINGS_INDEX + (2 * offset);
    if (!*entry) return entry;
    if (StrEq(b, e, LiteralName(MakeLiteral(entry)))) return entry; /* found! */
  }
  return 0;
}

Sym Insert(const char *b, const char *e, unsigned value, int allow_existing) {
  unsigned *entry = Lookup(b, e);
  THROW_UNLESS(entry, STATUS_OOM, 3);
  if (allow_existing && *entry) return MakeLiteral(entry);

  THROW_UNLESS(!*entry, STATUS_EVAL, MakeLiteral(entry));
  THROW_UNLESS(TP + (e - b + 1) <= STRINGS_SIZE, STATUS_OOM, 3);
  entry[0] = TP;
  entry[1] = value;
  while (b != e)
    STRINGS_TABLE[TP++] = *b++;
  STRINGS_TABLE[TP++] = 0;
  TP += 3u;
  TP &= ~3u;
  return MakeLiteral(entry);
}

/* lexer */
/* Special "symbol" values returned by lexer.
   They correspond to page symbols and cannot be produced by a lexeme. */
#define TOK_LPAREN ((0u << 2) | 3u)
#define TOK_RPAREN ((1u << 2) | 3u)

static Sym Lex(void) {
  unsigned c;
  do
    THROW_UNLESS(PC < PAGE_TABLE[PAGE_PROGRAM].end, STATUS_LEX, 3);
  while ((c = *PC++) <= ' ');

  switch (c) {
  case '(':
    return TOK_LPAREN;
  case ')':
    return TOK_RPAREN;
  }

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
  --PC;

  if (token_val != TOKEN_VAL_NONE) {
    return MakeUnsigned(token_val);
  }
  return Insert(token_begin, PC, SYM_NO_ASSOC, 1);
}

/* list constructor */
static inline Sym CONS(Sym car, Sym cdr) {
  STACK[--SP] = cdr;
  STACK[--SP] = car;
  THROW_UNLESS(PP < SP, STATUS_OOM, 3);
  return (SP << 2) | 1;
}
static inline unsigned CdrCell(Sym li) {
  ASSERT(IsCons(li));
  return (li >> 2) + 1;
}

/* parser */
static Sym ParseObject(Sym tok) {
  if (IsAtom(tok)) return tok;
  THROW_UNLESS(tok == TOK_LPAREN, STATUS_PARSE, 3);
  if ((tok = Lex()) == TOK_RPAREN) return SYM_NIL;
  unsigned low_mark = PP;
  while (tok != TOK_RPAREN) {
    THROW_UNLESS(++PP < SP, STATUS_PARSE, tok);
    STACK[PP - 1] = ParseObject(tok);
    tok = Lex();
  }
  Sym res = CONS(STACK[--PP], SYM_NIL);
  while (low_mark < PP)
    res = CONS(STACK[--PP], res);
  return res;
}

/* eval */
static Sym Eval(Sym e, Sym a);

static inline Sym CAR(Sym a) {
  THROW_UNLESS(IsCons(a), STATUS_EVAL, a);
  return STACK[(a >> 2) + 0];
}
static inline Sym CDR(Sym a) {
  THROW_UNLESS(IsCons(a), STATUS_EVAL, a);
  return STACK[(a >> 2) + 1];
}

static inline Sym EQ(Sym x, Sym y) {
  return IsAtom(x) && x == y ? SYM_T : SYM_NIL;
}
static inline Sym CAAR(Sym a) { return CAR(CAR(a)); }
static inline Sym CADR(Sym a) { return CAR(CDR(a)); }
static inline Sym CDAR(Sym a) { return CDR(CAR(a)); }
static inline Sym CDDR(Sym a) { return CDR(CDR(a)); }

/* creates a dense copy of object `s`.
 * offset should already be shifted by 2 bits
 */
static Sym Gc(Sym s, Sym mark, unsigned offset) {
  return (IsCons(s) && s < mark)
             ? CONS(Gc(CAR(s), mark, offset), Gc(CDR(s), mark, offset)) + offset
             : s;
}

static Sym Concat(Sym x, Sym y) {
  return !IsNil(x) ? CONS(CAR(x), Concat(CDR(x), y)) : y;
}
static Sym Evcon(Sym c, Sym a) {
  return Eval(CAAR(c), a) ? Eval(CAR(CDAR(c)), a) : Evcon(CDR(c), a);
}
static Sym Evlis(Sym m, Sym a) {
  return !IsNil(m) ? CONS(Eval(CAR(m), a), Evlis(CDR(m), a)) : SYM_NIL;
}

static inline Sym Pair(Sym a, Sym b) {
  return CONS(a, b); /* NOTE: must be in sync with Assoc */
}
static inline Sym Zip(Sym x, Sym y) {
  return !IsNil(x) ? CONS(Pair(CAR(x), CAR(y)), Zip(CDR(x), CDR(y))) : SYM_NIL;
}
static Sym Assoc(Sym e, Sym a) {
  if (IsUnsigned(e)) return e;
  while (CAAR(a) != e)
    a = CDR(a);
  return CDAR(a);
}

static Sym Let(Sym e, Sym a) {
  Sym vars = CAR(e);
  Sym body = CADR(e);
  Sym as, var, val;
  while (!IsNil(vars)) {
    as = CAR(vars);
    vars = CDR(vars);

    var = CAR(as);
    val = CADR(as);

    /* forbid shadowing core symbols */
    THROW_UNLESS(IsLiteral(var) && *LiteralAssoc(var) != SYM_SHADOW_FORBIDDEN,
                 STATUS_EVAL, var);
    a = CONS(Pair(var, Eval(val, a)), a);
  }
  return Eval(body, a);
}

static Sym Eval(Sym e, Sym a) {
  if (IsAtom(e)) return Assoc(e, a);

  Sym car = CAR(e), cdr = CDR(e);

  unsigned high_mark = SP;
  /* TODO: make switch */
  if (car == SYM_QUOTE) return CAR(cdr);

  if (car == SYM_ATOM)
    e = IsAtom(Eval(CAR(cdr), a)) ? SYM_T : SYM_NIL;
  else if (car == SYM_CAR)
    e = CAR(Eval(CAR(cdr), a));
  else if (car == SYM_CDR)
    e = CDR(Eval(CAR(cdr), a));
  else if (car == SYM_EQ)
    e = EQ(Eval(CAR(cdr), a), Eval(CADR(cdr), a));
  else if (car == SYM_CONS)
    e = CONS(Eval(CAR(cdr), a), Eval(CADR(cdr), a));
  else if (car == SYM_COND)
    e = Evcon(cdr, a);
  else if (car == SYM_LET)
    e = Let(cdr, a);
  else if (IsAtom(car))
    e = Eval(CONS(Assoc(car, a), cdr), a);
  else {
    THROW_UNLESS(CAR(car) == SYM_LAMBDA, STATUS_EVAL, car);
    e = Eval(CADR(CDR(car)), Concat(Zip(CADR(car), Evlis(cdr, a)), a));
  }

  unsigned low_mark = SP;
  e = Gc(e, (high_mark << 2) | 1, (high_mark - low_mark) << 2);
  const unsigned lowest_mark = SP;
  while (lowest_mark < low_mark)
    STACK[--high_mark] = STACK[--low_mark];
  return e;
}

#define TAR_CONTENT_OFFSET 512u
static void lispm_main(struct Page *program) {
  PAGE_TABLE = page_alloc(PAGE_TABLE_SIZE);
  THROW_UNLESS(PAGE_TABLE, STATUS_OOM, 3);
  PAGE_TABLE[PAGE_PROGRAM] = *program;
  PC = program->begin + TAR_CONTENT_OFFSET;

  InitStack();
  InitTable();

  Sym sym = ParseObject(Lex());
  DUMP(sym);
  sym = Eval(sym, SYM_NIL);
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

  if (IsUnsigned(sym)) {
    fprintf(stderr, "%u\n", sym >> 2);
  } else if (IsLiteral(sym)) {
    fprintf(stderr, "%s\n", LiteralName(sym));
  } else if (IsPage(sym)) {
    fprintf(stderr, "<page %u>\n", (sym >> 2));
  } else {
    fprintf(stderr, "(");
    indent += 2;
    same_line = 1;
    while (IsCons(sym)) {
      lispm_dump(CAR(sym));
      sym = CDR(sym);
    }
    if (!IsNil(sym)) {
      lispm_dump(sym);
    }
    indent -= 2;
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
    fprintf(stderr, !IsNil(sym) ? "!)\n" : ")\n");
  }
}
#endif
