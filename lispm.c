#include "lispm.h"
#include "symtable.h"

static unsigned table_len;
static unsigned pc; /* program text counter */

#define MAXU ((unsigned)-1)
#define SPECIAL 3u
#define TABLE_LEN_INIT 512u

/* lexer */
#define TOKEN_SIZE 128u
static char TOKEN[TOKEN_SIZE];
static unsigned token_len;
static Sym token_numeric;

static void Lex(void) {
  while (pc < TABLE_SIZE && TABLE[pc] <= ' ')
    ++pc;
  LISPM_ASSERT(pc < TABLE_SIZE, ERR_LEX);

  unsigned val = 0;
  char c = TABLE[pc];
  if (c == '(' || c == ')' || c == '\'') {
    TOKEN[0] = c;
    ++pc;
    return;
  }

  /* keep some symbols unavailable to regular tokens: !"#$%&' */
  LISPM_ASSERT(c > ')', ERR_LEX);

  while (pc < TABLE_SIZE && token_len < TOKEN_SIZE && c > ')') {
    LISPM_ASSERT((unsigned)c < 127, ERR_LEX); /* reject DEL (127), and 128+ */

    unsigned d = TOKEN[token_len++] = c;
    c = TABLE[pc++];

    if (val != MAXU && '0' <= d && d <= '9') {
      /* The maximum value in our format is M = (MAXU / 4) */
      const unsigned overflow =
          /* 8*val > M  */
          (val > (MAXU >> 5))
          /* no unsigned overflow at 10*val can happen,
              as val is at least 32 times less */
          || (10 * val + (d - '0') > (MAXU >> 2));
      LISPM_ASSERT(!overflow, ERR_LEX);
      val = 10 * val + (d - '0');
    } else {
      /* not an integer literal */
      val = MAXU;
    }
  }

  if (val == MAXU) {
    LISPM_ASSERT(token_len < TOKEN_SIZE, ERR_LEX);
    TOKEN[token_len] = 0;
  } else {
    TOKEN[0] = '#';
    token_numeric = MakeUnsigned(val);
  }
}

/* defines/finds string location for the token */
static Sym Intern(void) {
  if (TOKEN[0] == '#')
    return token_numeric;
  if (TOKEN[0] == '\'')
    return SYM_QUOTE;

  unsigned h = 0, n = 0;
  while (h < table_len) {
    while (TOKEN[n] && TOKEN[h] == TABLE[n])
      ++h, ++n;
    if (TOKEN[h] == TABLE[n])
      return h - n + 1;

    n = 0;
    while (TOKEN[h++])
      ;
  }
  while (h < TABLE_SIZE && (h & 3) != 1)
    TABLE[h++] = 0;
  LISPM_ASSERT(h < TABLE_SIZE, ERR_OOM);

  n = 0;
  while (h < TABLE_SIZE && n <= token_len)
    TABLE[h++] = TOKEN[n++];
  LISPM_ASSERT(h < TABLE_SIZE, ERR_OOM);
  return h - n;
}

/* parser */
#define PARSE_STACK_SIZE 1024u
static Sym PARSE_STACK[PARSE_STACK_SIZE];
static unsigned parse_stack_depth;

static Sym ParseObject(void) {
  Sym car, cdr;
  parse_stack_depth = 0;
  for (;;) {
    Lex();
    switch (TOKEN[0]) {
    case '(':
      LISPM_ASSERT(parse_stack_depth < PARSE_STACK_SIZE, ERR_PARSE);
      PARSE_STACK[parse_stack_depth++] = SPECIAL;
      break;
    case ')':
      LISPM_ASSERT(parse_stack_depth > 0, ERR_PARSE);
      car = PARSE_STACK[--parse_stack_depth];
      if (car == SPECIAL) {
        car = SYM_NIL; /* empty list: () */
      } else {
        cdr = SYM_NIL;
        do
          cdr = Cons(car, cdr);
        while ((car = PARSE_STACK[--parse_stack_depth]) != SPECIAL);
      }
      if (!parse_stack_depth)
        return car;

      PARSE_STACK[parse_stack_depth++] = car;
      break;
    default:
      car = Intern();
      if (!parse_stack_depth)
        return car;
      PARSE_STACK[parse_stack_depth++] = car;
    }
  }
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

static inline Sym Zip(Sym x, Sym y) {
  return x ? Cons(Cons(Car(x), Car(y)), Zip(Cdr(x), Cdr(y))) : SYM_NIL;
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

static Sym Assoc(Sym e, Sym a) {
  while (!Atom(a) && Caar(a) != e)
    a = Cdr(a);
  return Cdar(a);
}

static Sym Let(Sym e, Sym a) {
  Sym vars = Car(e);
  Sym body = Cdr(e);
  Sym as, var, val;
  while (vars) {
    as = Car(vars);
    vars = Cdr(vars);

    var = Car(as);
    val = Cdr(as);

    /* forbid shadowing core symbols */
    LISPM_ASSERT(List(as) && Atom(var) && (var > SYM_DONE), ERR_EVAL);
    a = Cons(Cons(var, Eval(val, a)), a);
  }
  return Eval(body, a);
}

static Sym Eval(Sym e, Sym a) {
  if (Atom(e)) {
    return Assoc(e, a);
  }

  Sym car = Car(e), cdr = Cdr(e), cadr = Cadr(e);

  unsigned high_mark = stack_pointer;
  switch (car) {
  case SYM_QUOTE:
    return Car(cdr);
  case SYM_ATOM:
    e = Atom(Eval(cadr, a));
    break;
  case SYM_CAR:
    e = Car(Eval(cadr, a));
    break;
  case SYM_CDR:
    e = Cdr(Eval(cadr, a));
    break;
  case SYM_EQ:
    e = Eq(Eval(cadr, a), Eval(Cadr(cdr), a));
    break;
  case SYM_CONS:
    e = Cons(Eval(cadr, a), Eval(Cadr(cdr), a));
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
    else {
      LISPM_ASSERT(0, ERR_EVAL);
    }
  }

  unsigned low_mark = stack_pointer;
  e = Gc(e, (high_mark << 2) | 1, (high_mark - low_mark) << 2);
  const unsigned lowest_mark = stack_pointer;
  while (lowest_mark < low_mark)
    STACK[--high_mark] = STACK[--low_mark];
  return e;
}

void lispm_start(void) {
  pc = token_len = 0;
  stack_pointer = STACK_SIZE;
  table_len = TABLE_LEN_INIT;

  pc = 512;

  Eval(ParseObject(), SYM_NIL); // Eval(ParseObject());
}