#include "debug.h"
#include "lispm.h"
#include "sym.h"

#include <stdio.h>

void lispm_dump(const struct PageDesc *table, Sym sym) {
  static int indent = 0;
  static int same_line = 0;

  const unsigned *index = table[page_pt_offs(PAGE_INDEX)].begin;
  const unsigned *stack = table[page_pt_offs(PAGE_STACK)].begin;
  const char *strings = table[page_pt_offs(PAGE_STRINGS)].begin;

  if (!same_line)
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
  same_line = 0;

  if (sym == SYM_NIL) {
    fprintf(stderr, "()\n");
  } else if (sym == SYM_T) {
    fprintf(stderr, "T\n");
  } else if (is_unsigned(sym)) {
    fprintf(stderr, "%u\n", sym >> 2);
  } else if (is_literal(sym)) {
    fprintf(stderr, "%s\n", strings + (index[literal_ht_offs(sym)]));
  } else if (is_special(sym)) {
    fprintf(stderr, "<special %x>\n", sym);
  } else if (is_cons(sym)) {
    fprintf(stderr, "(");
    indent += 2;
    same_line = 1;
    while (is_cons(sym)) {
      Sym car = stack[st_obj_st_offs(sym)];
      sym = stack[st_obj_st_offs(sym) + 1];
      lispm_dump(table, car);
    }
    if (sym != SYM_NIL) lispm_dump(table, sym);

    indent -= 2;
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
    fprintf(stderr, sym != SYM_NIL ? "!)\n" : ")\n");
  } else if (is_lambda(sym)) {
    unsigned offs = st_obj_st_offs(sym);
    Sym cap = stack[offs], par = stack[offs + 1], body = stack[offs + 2];
    fprintf(stderr, "(LAMBDA\n");
    indent += 2;
    lispm_dump(table, par);
    lispm_dump(table, body);
    lispm_dump(table, cap);
    indent -= 2;
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
    fprintf(stderr, ")\n");
  }
}
