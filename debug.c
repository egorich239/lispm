#include "debug.h"
#include "lispm.h"
#include "sym.h"

#include <stdio.h>

static const char *literal_name(Sym l) {
  Sym pg, offs, len, p = lispm_literal_name_span(l);
  lispm_span_unpack(p, &pg, &offs, &len);
  return lispm_page_loc(pg, lispm_unsigned_val(offs), 1);
}

void lispm_dump(const struct PageDesc *table, Sym sym) {
  static int indent = 0;
  static int same_line = 0;

  const unsigned *stack = table[lispm_page_pt_offs(LISPM_PAGE_STACK)].begin;

  if (!same_line)
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
  same_line = 0;

  if (sym == LISPM_SYM_NIL) {
    fprintf(stderr, "()\n");
  } else if (sym == LISPM_SYM_T) {
    fprintf(stderr, "T\n");
  } else if (lispm_sym_is_unsigned(sym)) {
    fprintf(stderr, "%u\n", lispm_unsigned_val(sym));
  } else if (lispm_sym_is_literal(sym)) {
    fprintf(stderr, "%s\n", literal_name(sym));
  } else if (lispm_sym_is_special(sym)) {
    fprintf(stderr, "<special %x>\n", sym);
  } else if (lispm_sym_is_cons(sym)) {
    fprintf(stderr, "(");
    indent += 2;
    same_line = 1;
    while (lispm_sym_is_cons(sym)) {
      Sym car = stack[lispm_st_obj_st_offs(sym)];
      sym = stack[lispm_st_obj_st_offs(sym) + 1];
      lispm_dump(table, car);
    }
    if (sym != LISPM_SYM_NIL) lispm_dump(table, sym);

    indent -= 2;
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
    fprintf(stderr, sym != LISPM_SYM_NIL ? "!)\n" : ")\n");
  } else if (lispm_sym_is_lambda(sym)) {
    unsigned offs = lispm_st_obj_st_offs(sym);
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

const char *lispm_error_message_get(const struct PageDesc *table) {
  return table[lispm_page_pt_offs(LISPM_PAGE_STRINGS)].begin;
}