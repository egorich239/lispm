#pragma once

#include "lispm.h"
#include <stdio.h>

extern Sym* STACK;

static inline void lispm_dump(Sym sym) {
  static int indent = 0;
  static int same_line = 0;

  if (!same_line)
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
  same_line = 0;

  if (Unsigned(sym)) {
    fprintf(stderr, "%u\n", sym);
  } else if (Literal(sym)) {
    fprintf(stderr, "%s\n", LiteralName(sym));
  } else if (Special(sym)) {
    fprintf(stderr, "<special %x>\n", sym);
  } else {
    fprintf(stderr, "(");
    indent += 2;
    same_line = 1;
    while (List(sym)) {
      lispm_dump(STACK[sym >> 2]);
      sym = STACK[(sym >> 2) + 1];
    }
    if (sym) {
      lispm_dump(sym);
    }
    indent -= 2;
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
    fprintf(stderr, sym ? "!)\n" : ")\n");
  }
}
