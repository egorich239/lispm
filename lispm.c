#include "lispm.h"
#include "rt.h"
#include "symtable.h"

/* Unlike LISPM_ASSERT, these errors are caused by a bug in the user code. */
#define THROW_UNLESS(cond, err)                                                \
  do {                                                                         \
    if (!(cond)) {                                                             \
      sym = (err);                                                             \
      THROW(1);                                                                \
    }                                                                          \
  } while (0)

/* state */
static struct Page program_page;
static const char *pc; /* program text counter */

Sym *STACK;
static unsigned sp; /* stack pointer, grows down */

static unsigned tp; /* table pointer, grows up */
static Sym sym;     /* adjusted by lexer, parser, and eval */

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

/* Hash table
 *
 * Hash table entry is simply a pointer to an 8-byte aligned storage of two L64
 * ints.
 *
 * The first word K contains the offset of the literal, its highest L64 bit is
 * set to mark the presence of the entry, the literal is found at TABLE +
 * (masked(K) << 2), where masked(K) = K & (L64_MAX >> 1).
 */
typedef char *Entry;
#define HTABLE_ENTRY_SIZE (2u * L64_CSIZE)
#define HTABLE_ENTRY(k)                                                        \
  (TABLE + HTABLE_OFFSET + HTABLE_ENTRY_SIZE * ((k) & (HTABLE_SIZE - 1)))

/* Return value of:
   - lexer
   - parser.

   Both of these functions are non-recursive, which allows us to do that.
*/
static Entry entry;

#define HTABLE_MAKE_KEY(s) (((s) >> 2) | (L64_MAX ^ (L64_MAX >> 1)))
#define HTABLE_GET_SYM(k) (((k) & ~(L64_MAX ^ (L64_MAX >> 1))) << 2)

/* The error values are less than HTABLE_OFFSET */
#define HTABLE_NONE MAKE_SPECIAL(0)
#define HTABLE_OOM MAKE_SPECIAL(1)

static void InitTable(void);
static void Lookup(const char *lit);
void Update(const char *lit, const char *hidden, int align_log2);

static void InitTable(void) {
  tp = 0;
  for (unsigned t = 0; t < HTABLE_OFFSET; ++t) {
    if (!TABLE[t]) continue;
    Update(TABLE + t, 0, 2);
    while (TABLE[t++])
      ;
    --t;
  }
  tp = HTABLE_ENTRY(HTABLE_SIZE - 1) + HTABLE_ENTRY_SIZE - TABLE;
}

static void Lookup(const char *lit) {
  unsigned offset = Djb2(lit);
  const unsigned end_offset = offset + HTABLE_SIZE;

  LOG("looking for %s starting at %u\n", lit, offset % HTABLE_SIZE);
  do {
    entry = HTABLE_ENTRY(offset);
    unsigned key = FromL64(entry);
    if (!key) {
      /* empty slot */
      sym = HTABLE_NONE;
      return;
    }
    sym = HTABLE_GET_SYM(key);
    if (StrEq(lit, TABLE + sym)) return; /* found! */
  } while (++offset != end_offset);
  sym = HTABLE_OOM;
}

static unsigned StrToTableAligned(const char *str, unsigned align) {
  tp += align - 1;
  tp &= ~(align - 1);
  unsigned result = tp;
  do
    if (tp == TABLE_SIZE) {
      sym = HTABLE_OOM;
      return result;
    }
  while ((TABLE[tp++] = *str++));
  return result;
}

void Update(const char *lit, const char *hidden, int align_log2) {
  Lookup(lit);
  if (sym == HTABLE_OOM) return;

  if (hidden)
    ToL64(entry + L64_CSIZE,
          StrToTableAligned(hidden, 1 << align_log2) >> align_log2);
  if (sym != HTABLE_NONE) return;

  const unsigned key = HTABLE_MAKE_KEY(StrToTableAligned(lit, 4));
  sym = HTABLE_GET_SYM(key);
  ToL64(entry, key);
  LOG("inserted symbol %u: %s\n", sym, LiteralName(sym));
}

/* lexer */
#define TOKEN_SIZE 128u
static char TOKEN[TOKEN_SIZE];

#define TOK_LPAREN MAKE_SPECIAL('(')
#define TOK_RPAREN MAKE_SPECIAL(')')

static void Lex(void) {
  unsigned c;
  do
    THROW_UNLESS(pc < program_page.end, ERR_LEX);
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
    THROW_UNLESS(pc < program_page.end, ERR_LEX);
  } while ((c = *pc++) > ')');
  --pc;

  if (token_val != TOKEN_VAL_NONE) {
    sym = MakeUnsigned(token_val);
    return;
  }

  THROW_UNLESS(token_len < TOKEN_SIZE, ERR_LEX);
  TOKEN[token_len] = 0;
  Update(TOKEN, 0, 4);
  THROW_UNLESS(sym != HTABLE_OOM, ERR_LEX);
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
    THROW_UNLESS(Atom(var) && (var >= HTABLE_OFFSET), ERR_EVAL);
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

static void lispm_main(void) {
  InitStack();
  InitTable();

  ParseObject();

  LOG("parse result:%s\n", "");
  DUMP(sym);
  sym = Eval(sym, SYM_NIL);
}

#define TAR_CONTENT_OFFSET 512u

Sym lispm_start(struct Page *program) {
  program_page = *program;

  pc = program->begin + TAR_CONTENT_OFFSET;
  TRY(lispm_main());
  return sym;
}