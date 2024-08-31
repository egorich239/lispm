#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include "debug.h"
#include "lispm.h"

extern struct Builtin lispm_builtins[];

extern const char _binary_evaltests_txt_start[];
extern const char _binary_evaltests_txt_end[];

Sym stack[4 * 1024 * 1024];
char strings[16 * 1024 * 1024];
unsigned htable[4 * 1024 * 1024];

static int sym_equal(Sym a, Sym b) {
  if (lispm_sym_is_atom(a) && lispm_sym_is_atom(b)) return a == b;
  if (lispm_sym_is_error(a) && lispm_sym_is_error(b)) return a == b;
  if (lispm_sym_is_atom(a) || lispm_sym_is_atom(b)) return 0;
  if (!lispm_sym_is_cons(a) || !lispm_sym_is_cons(b)) return 0;

  Sym *aa = lispm_st_obj_unpack(a), *bb = lispm_cons_unpack_user(b);
  if (!sym_equal(aa[0], bb[0])) return 0;
  return sym_equal(aa[1], bb[1]);
}

struct Lispm lispm = {
    .builtins = lispm_builtins,

    /*stack*/
    .stack = stack,
    .sp = stack + (sizeof(stack) / sizeof(*stack)),
    .pp = stack + LISPM_PP_OFFSET,

    /*strings*/
    .strings = strings,
    .strings_end = strings + sizeof(strings),
    .tp = strings,

    /*program*/
    .program = _binary_evaltests_txt_start,
    .program_end = _binary_evaltests_txt_end,
    .pc = _binary_evaltests_txt_start,

    /*htable*/
    .htable = htable,
    .htable_end = htable + (sizeof(htable) / sizeof(*htable)),

#if LISPM_CONFIG_VERBOSE
    .trace = {},
#endif
};

#define M lispm

/* we have to wrap parse calls into try block, otherwise lex/parse error segfault */
static Sym parse_result;
static void parse_void(void) {
  parse_result = LISPM_ERR_PARSE;
  parse_result = lispm_parse(M.pc, M.program_end);
}

int main(int argc, char *argv[]) {
  int result = 0;
  int comment = 0;
  lispm_init();
  lispm_trace();
  for (; M.pc < M.program_end; ++M.pc) {
    if (*M.pc == ';') comment = 1;
    if (comment) {
      if (*M.pc == '\n') comment = 0;
      continue;
    }
    if (*M.pc <= ' ') continue;
    Sym testname;
    lispm_rt_try(parse_void);
    testname = parse_result;
    if (!lispm_sym_is_literal(testname)) {
      fprintf(stderr, "Expected a test name literal, got: ");
      lispm_print_short(testname);
      fprintf(stderr, "\nThe layout of the test file is probably broken; terminating\n");
      return 1;
    }
    Sym actual = lispm_exec();
    Sym expected;
    lispm_rt_try(parse_void);
    expected = parse_result;

    if (actual == LISPM_ERR_PARSE || expected == LISPM_ERR_PARSE) {
      fprintf(stderr, "Lexing or parsing error encountered; terminating\n");
      return 1;
    }

    lispm_print_short(testname);
    if (sym_equal(actual, expected)) {
      fprintf(stderr, ": OK\n");
    } else {
      result = 1;
      fprintf(stderr, ": FAIL\n  expected: ");
      lispm_print_short(expected);
      fprintf(stderr, "\n  actual: ");
      lispm_print_short(actual);
      fprintf(stderr, "\n");
    }
  }
  return result;
}