#include <stdio.h>
#include <string.h>

static const char *SYMS[] = {
    "NIL",          //
    "QUOTE",        //
    "COND",         //
    "LET",          //
    "LAMBDA",       //
    "T",            //
    "EQ",           //
    "ATOM",         //
    "CAR",          //
    "CDR",          //
    "CONS",         //
    "STR",          //
    "!LEX",         //
    "!PARSE",       //
    "!EVAL",        //
    "!OOM",         //
    "!INVALID_SYM", //
};
#define SYMS_SIZE (sizeof(SYMS) / sizeof(const char *))

void print(const char *s) { fwrite(s, 1, strlen(s), stdout); }

int main() {
  print("#pragma once\n\n");
  print("#define TABLE_PREFIX \\\n");
  unsigned offs = 0;
  for (unsigned i = 0; i < SYMS_SIZE; ++i) {
    fputc('"', stdout);
    print(SYMS[i]);
    offs += strlen(SYMS[i]);
    do
      print("\\0"), ++offs;
    while (offs & 3);
    print("\" \\\n");
  }
  fprintf(stdout, "\n#define HTABLE_OFFSET %u\n\n", (offs + 511) & ~511u);

  offs = 0;
  for (unsigned i = 0; i < SYMS_SIZE; ++i) {
    if (*SYMS[i] != '!') {
      fprintf(stdout, "#define SYM_%s %uu\n", SYMS[i], offs);
    } else {
      fprintf(stdout, "#define ERR_%s %uu\n", SYMS[i] + 1, offs);
    }
    offs += (strlen(SYMS[i]) & ~3u) + 4;
  }
}