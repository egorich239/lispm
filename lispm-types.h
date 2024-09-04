#pragma once

#include "lispm-config.h"

/** Symbol, one of:
 *  - NIL:            corresponds to empty list
 *  - short unsigned: an atom, corresponding to an unsigned integer
 *                    shorter than native unsigned by two bits;
 *  - literal:        an atom, corresponding to a valid literal;
 *  - stack object:   CONS, LAMBDA, full size unsigned;
 *  - special symbol: T, builtins, errors, implementation values.
 *
 * The general layout takes some lower and upper bits for special information,
 * we describe it for 32-bit unsigned, but same goes for 64-bit unsigned,
 * only the payload fields get wider.
 *
 * Layout:
 * -               0: LISPM_SYM_NIL
 * -    <OFFS:30> 00: literal with the given <OFFS> in htable;
 *                    each literal consumes two words in htable:
 *                    0: <LIT:30> w0 - bit 'w' is 0 for readonly literals, else 1;
 *                                     LIT is offset of NUL-terminated representation
 *                                     of literal in strings table.
 *                    1: the currently assigned value of the literal.
 * -     <NUM:30> 01: short inline unsigned;
 * - <OFFS:28> 0s 10: stack pointer, the object consume '2+s' consequtive words on stack,
 *                    starting with <OFFS>;
 *   Stack objects:
 * -    <CONS> 00 10: pair car, cdr;
 * -  <LAMBDA> 01 10: triplet captures, args, body;
 * -    <SPTR> 10 10: an extension stack object, consisting of exactly TWO words starting
 *                    at SPTR position;
 * -    <SPTR> 11 10: an extension stack object, consisting of exactly THREE words starting
 *                    at SPTR position;
 *
 * Extensions must adhere to the symbol layout in the part of the stack they use
 * (i.e they must put valid symbols on the stack), because garbage collector assumes that
 * everything it ever observes on the stack is some kind of Sym.
 *
 *   Special symbols.
 *
 * -    <N:28> 00 11: builtin function at the offset N in builtins table;
 *
 * -       ... 11 11: special values
 */
typedef unsigned Sym;

enum { LISPM_SYM_NIL = 0 };

/* State of LISPM */
struct Lispm {
  /* Location of the stack bottom. */
  Sym *stack, *stack_end;
  /* Stack pointer. Grows down. */
  Sym *sp;

  /* Beginning of the strings storage. */
  char *strings, *strings_end;
  /* Pointer past the end of the used part of string storage.
     Initialized by lispm_init(). */
  char *tp;

  /* Beginning of the program page. */
  const char *program, *program_end;
  /* Pointer to the next lexeme. */
  const char *pc;

  /* Beginning of hash table */
  unsigned *htable, *htable_end;

  /* Internal values */
  unsigned htable_index_size;
  int htable_index_shift;
  Sym parse_frame;
  unsigned frame_depth;
};

/* tracing */
struct LispmTraceCallbacks {
  void (*apply_enter)(Sym fn, Sym fn_resolved, Sym args);
  void (*apply_leave)(void);
  void (*lambda_proto)(Sym lambda);
  void (*lambda_cons)(Sym lambda);

  void (*assertion)(const char *file, unsigned line, const char *msg);
  void (*panic)(const char *file, unsigned line, const char *msg, Sym ctx);
  void (*lex_error)(const char *file, unsigned line);
  void (*parse_error)(const char *file, unsigned line, Sym tok);
  void (*oom_stack)(const char *file, unsigned line);
  void (*oom_htable)(const char *file, unsigned line);
  void (*oom_strings)(const char *file, unsigned line);
  void (*unbound_symbol)(const char *file, unsigned line, Sym sym);
  void (*illegal_bind)(const char *file, unsigned line, Sym sym);
};
