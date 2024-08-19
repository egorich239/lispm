#pragma once

#include "symtable.h"

#define LISPM_ASSERT(cond)                                                     \
  if (!(cond)) __builtin_trap()

#define PAGE_ALIGN_LOG2 12
#define PAGE_ALIGN (1 << PAGE_ALIGN_LOG2)

/* Page must be aligned to PAGE_ALIGN.
 */
struct Page {
  char* begin;
  char* end;
};

#define TABLE_SIZE 0x100000u
_Static_assert((TABLE_SIZE & 4095) == 0,
               "main table size must be proportional to 4K pages");

/* L64 integers are 4-byte char sequences, that are interpreted as integers.
 *
 * Each byte is considered a 64-base digit (c % 64).
 *
 * The canonical representation of digits consists of chars in [32; 95] range.
 * An important property of the numbers is that NUL char maps to digit 0,
 * so that L64 int is zero for NULx4 sequence.
 */
#define L64_CSIZE 4
#define L64_MAX ((1u << 24) - 1u)

static inline unsigned FromL64(const char *c) {
  unsigned acc = 0;
  for (int i = 0; i < L64_CSIZE; ++i) {
    const unsigned d = ((unsigned)c[i]) & 63;
    acc <<= 6;
    acc += d;
  }
  return acc;
}

static inline void ToL64(char *target, unsigned v) {
  for (int i = 0; i < L64_CSIZE; ++i) {
    const unsigned d = (v & 63);
    v >>= 6;
    target[L64_CSIZE - 1 - i] = (d < 32) ? d + 64 : d;
  }
  LISPM_ASSERT(!v);
}

/* we expect TABLE initialized with TABLE_PREFIX from symtable.h */
extern char TABLE[];

#define HTABLE_SIZE 1024u
_Static_assert((HTABLE_SIZE & (HTABLE_SIZE - 1)) == 0,
               "hash table size must be a power of two");
_Static_assert(HTABLE_OFFSET + 2 * L64_CSIZE <= TABLE_SIZE,
               "main table is too small");

/* symbol: last two bits encode its kind
 * - <OFFS> 00: symbol from the table, at '<OFFS> 00' position;
 * -  <NUM> 10: for tokens that represent decimal representation of bits of
 * <NUM>;
 * - <CONS> 01: (CONS car cdr), stored at stack CONS (car) and CONS+1 (cdr)
 * position;
 * - <BITS> 11: special marker.
 */
typedef unsigned Sym;

/* stack */
#define STACK_SIZE 0x10000u
extern unsigned stack_pointer;
extern Sym STACK[];

/* capi callback, if present */
extern Sym (*lispm_capi)(Sym (*eval)(Sym, Sym), Sym, Sym);

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
  LISPM_ASSERT(!(val & ~(~0u >> 2)));
  return (val << 2) | 2;
}
static inline unsigned GetUnsigned(Sym val) {
  LISPM_ASSERT(Unsigned(val));
  return val >> 2;
}

static inline const char *LiteralName(Sym x) {
  LISPM_ASSERT(Literal(x));
  return TABLE + x;
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

#if 1
#include "symprint.h"
#include <stdio.h>

#define LOG(fmt, ...) ((void)(fmt))
// #define DUMP(sym) ((void)(sym))
#define DUMP(sym) lispm_dump(sym)
#else
#include "symprint.h"
#include <stdio.h>
#define LOG(fmt, ...) fprintf(stderr, "[%d] " fmt, __LINE__, __VA_ARGS__)
#define DUMP(sym) lispm_dump(sym)
#endif
