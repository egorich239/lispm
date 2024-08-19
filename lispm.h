#pragma once

#include "rt.h"
#include "symtable.h"

/* Page must be aligned to CPU page size */
struct Page {
  char *begin;
  char *end;
};

#define PAGE_TABLE_SIZE 1024u

#define STRINGS_SIZE 0x100000u
_Static_assert(!(STRINGS_SIZE & 4095),
               "main table size must be proportional to 4K pages");

#define HTABLE_SIZE 1048576u
_Static_assert(!(HTABLE_SIZE & (HTABLE_SIZE - 1)),
               "hash table size must be a power of two");

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
