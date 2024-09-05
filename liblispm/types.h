#pragma once

#include <liblispm/config.h>

/** LispmObj, one of:
 *  - NIL:            corresponds to empty list
 *  - short unsigned: an atom, corresponding to an unsigned integer
 *                    shorter than native unsigned by two bits;
 *  - literal:        an atom, corresponding to a valid literal;
 *  - stack object:   CONS, LAMBDA, full size unsigned;
 *  - special object: builtins, implementation values.
 *
 * The general layout uses several lower bits as tags,
 * we the layout for 32-bit unsigned, but same goes for 64-bit unsigned,
 * only the payload fields get wider.
 *
 * Layout:
 * -               0: LISPM_SYM_NIL
 * -    <OFFS:30> 00: literal with the given <OFFS> in htable;
 *                    each literal consumes two words in htable:
 *                    0: <LIT:30> lR - bit 'l' is 0 for readonly literals, else 1;
 *                                     bit 'R' is 0 for literals that can be used as values, otherwise 1.
 *                       LIT is offset of NUL-terminated literal name in strings table.
 *                    1: the currently assigned value of the literal.
 * -     <NUM:30> 01: short inline unsigned;
 * - <OFFS:28> ss 10: stack pointer, the object consume '2+ss' consequtive words on stack,
 *                    starting with <OFFS>;
 *   Stack objects:
 * -    <CONS> 00 10: pair car, cdr;
 * -  <TRIPLE> 01 10: triplet, mostly used for lambdas; it is not recommended
 *                    to reuse triplets for extensions, as a user-constructed lambda could
 *                    then be easily confused for an extension object;
 * -    <QUAD> 10 10: an extension stack object, contains four words; it is recommended
 *                    to use `#tag` as the first element to differentiate between different
 *                    extensions;
 * -   <PENTA> 10 10: an extension stack object, contains five words; it is recommended
 *                    to use `#tag` as the first element to differentiate between different
 *                    extensions.
 *
 * If a stack object has recursive nature, then it is recommended to put the reference to
 * to the next object as the last element of the object. In that case garbage collector will
 * handle the recursive object without recursive calls (TODO: actually do this GC change).
 *
 * Extensions must adhere to the symbol layout in the part of the stack they use
 * (i.e they must put valid symbols on the stack), because garbage collector assumes that
 * everything it ever observes on the stack is some kind of LispmObj.
 *
 *   Special symbols.
 * -    <N:28> 00 11: builtin objects at the offset N in builtins table;
 * -       ... xx 11: special values.
 */
typedef unsigned LispmObj;

enum {
  /* Empty list object. */
  LISPM_SYM_NIL = 0,
};

/**
 * Stack objects allocate various space on the stack. All cells of all stack objects must be valid objects.
 *
 * All objects of size two are considered cons-objects.
 * All objects of size three are considered internal implementation detail of the VM. Users can create such objects with
 * `(lambda )` syntax.
 * Objects of size four and five are intended for extensions. It is recommended to put a type `#tag` for each extension
 * into its first cell.
 */
enum LispmStObjKind {
  LISPM_ST_OBJ_CONS = 2u,
  LISPM_ST_OBJ_TRIPLET = 6u,
  LISPM_ST_OBJ_QUAD = 10u,
  LISPM_ST_OBJ_PENTA = 14u,
};

/* State of LISPM */
struct Lispm {
  /* Location of the VM stack. */
  LispmObj *stack, *stack_end;

  /* Location of the VM strings storage. */
  char *strings, *strings_end;

  /* Location of the VM program page. */
  const char *program, *program_end;
  /* Pointer to the next lexeme, must point to the beginning
     of the program begore lispm_exec(). */
  const char *pc;

  /* Hash table storage. Must have a power-of-two size. */
  unsigned *htable, *htable_end;

  /* VM state, initialized by lispm_init(). */
  /* Stack pointer. Grows down. */
  LispmObj *sp;
  /* Pointer past the end of the used part of string storage. */
  char *tp;

  /* Size of hash table index, i.e. (htable - htable_end) / 2. */
  unsigned htable_index_size;
  /* Cached value of the right shift that hash function performs to get the
     offset in the table. */
  int htable_index_shift;

  /* Stores information about the names defined and used in
     the current lexical frame, used during semantic analysis. */
  LispmObj lex_frame;
  unsigned lex_frame_depth;
};

/* Callbacks, triggered on various events in the VM. */
struct LispmTraceCallbacks {
  void (*apply_enter)(LispmObj fn, LispmObj fn_resolved, LispmObj args);
  void (*apply_leave)(void);
  void (*lambda_proto)(LispmObj lambda);
  void (*lambda_cons)(LispmObj lambda);

  void (*assertion)(const char *file, unsigned line, const char *msg);
  void (*panic)(const char *file, unsigned line, const char *msg, LispmObj ctx);
  void (*lex_error)(const char *file, unsigned line);
  void (*parse_error)(const char *file, unsigned line, LispmObj tok);
  void (*oom_stack)(const char *file, unsigned line);
  void (*oom_htable)(const char *file, unsigned line);
  void (*oom_strings)(const char *file, unsigned line);
  void (*unbound_symbol)(const char *file, unsigned line, LispmObj sym);
  void (*illegal_bind)(const char *file, unsigned line, LispmObj sym);
};
