#include "debug.h"
#include "lispm.h"

#include <stdio.h>

static const char *literal_name(Sym l) { return lispm.strings + lispm_literal_str_offs(l); }

void lispm_print_short(Sym sym) {
  switch (sym) {
  case LISPM_SYM_NIL:
    fprintf(stderr, "()");
    return;
  case LISPM_SYM_T:
    fprintf(stderr, "t");
    return;
  case LISPM_ERR_OOM:
    fprintf(stderr, "!OOM");
    return;
  case LISPM_ERR_LEX:
    fprintf(stderr, "!LEX");
    return;
  case LISPM_ERR_PARSE:
    fprintf(stderr, "!PARSE");
    return;
  case LISPM_ERR_EVAL:
    fprintf(stderr, "!EVAL");
    return;
  }
  if (lispm_sym_is_literal(sym)) {
    fprintf(stderr, "%s", literal_name(sym));
    return;
  }
  if (lispm_sym_is_shortnum(sym)) {
    fprintf(stderr, "%u", lispm_shortnum_val(sym));
    return;
  }
  if (lispm_sym_is_longnum(sym)) {
    Sym *hilo = lispm_st_obj_unpack(sym);
    unsigned val = (lispm_shortnum_val(hilo[0]) << LISPM_SHORTNUM_BITS) | lispm_shortnum_val(hilo[1]);
    fprintf(stderr, "%u", val);
    return;
  }
  if (lispm_sym_is_builtin_sym(sym)) {
    fprintf(stderr, "<builtin %s>", lispm.builtins[lispm_builtin_sym_offs(sym)].name);
    return;
  }
  if (lispm_sym_is_special(sym)) {
    fprintf(stderr, "<special %x>", sym);
    return;
  }
  if (lispm_sym_is_lambda(sym)) {
    fprintf(stderr, "(lambda (...))");
    return;
  }
  LISPM_ASSERT(lispm_sym_is_cons(sym));
  int counter = 3;
  fprintf(stderr, "(");
  while (!lispm_sym_is_cons(sym)) {
    if (counter < 0) continue;
    if (counter == 0) {
      fprintf(stderr, " ...");
      --counter;
      continue;
    }
    if (counter-- < 3) { fprintf(stderr, " "); }
    Sym *cons = lispm_st_obj_unpack(sym);
    lispm_print_short(cons[0]);
    sym = cons[1];
  }
  fprintf(stderr, lispm_sym_is_nil(sym) ? ")" : "]");
}

void lispm_dump(Sym sym) {
  static int indent = 0;
  static int same_line = 0;

  const unsigned *stack = lispm.stack;

  if (!same_line)
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
  same_line = 0;

  if (lispm_sym_is_cons(sym)) {
    fprintf(stderr, "(");
    indent += 2;
    same_line = 1;
    while (lispm_sym_is_cons(sym)) {
      Sym car = stack[lispm_st_obj_st_offs(sym)];
      sym = stack[lispm_st_obj_st_offs(sym) + 1];
      lispm_dump(car);
    }
    if (sym != LISPM_SYM_NIL) lispm_dump(sym);

    indent -= 2;
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
    fprintf(stderr, sym != LISPM_SYM_NIL ? "]\n" : ")\n");
  } else if (lispm_sym_is_lambda(sym)) {
    unsigned offs = lispm_st_obj_st_offs(sym);
    Sym cap = stack[offs], par = stack[offs + 1], body = stack[offs + 2];
    fprintf(stderr, "(LAMBDA\n");
    indent += 2;
    lispm_dump(par);
    lispm_dump(body);
    lispm_dump(cap);
    indent -= 2;
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
    fprintf(stderr, ")\n");
  } else {
    lispm_print_short(sym);
    fprintf(stderr, "\n");
  }
}

const char *lispm_error_message_get(void) { return lispm.strings; }