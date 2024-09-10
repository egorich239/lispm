#include <stdio.h>
#include <stdlib.h>

#include <liblispm/debug.h>
#include <liblispm/lispm.h>

extern const char _binary_eval_data_txt_start[];
extern const char _binary_eval_data_txt_end[];

LispmObj stack[4 * 1024 * 1024];
char strings[16 * 1024 * 1024];
unsigned htable[4 * 1024 * 1024];

static int sym_equal(LispmObj a, LispmObj b) {
  if (lispm_obj_is_atom(a) && lispm_obj_is_atom(b)) return a == b;
  if (lispm_obj_is_atom(a) || lispm_obj_is_atom(b)) return 0;
  if (!lispm_obj_is_cons(a) || !lispm_obj_is_cons(b)) return 0;

  LispmObj *aa = lispm_obj_unpack(a), *bb = lispm_obj_unpack(b);
  if (!sym_equal(aa[0], bb[0])) return 0;
  return sym_equal(aa[1], bb[1]);
}

struct Lispm lispm = {
    /*stack*/
    .stack = stack,
    .stack_end = stack + (sizeof(stack) / sizeof(*stack)),

    /*strings*/
    .strings = strings,
    .strings_end = strings + sizeof(strings),
    .tp = strings,

    /*program*/
    .pc = _binary_eval_data_txt_start,
    .pc_end = _binary_eval_data_txt_end,

    /*htable*/
    .htable = htable,
    .htable_end = htable + (sizeof(htable) / sizeof(*htable)),

    /**/
    .stack_depth_limit = 16384u,
};

#define M lispm

static void parse_error(const char *file, unsigned line, LispmObj tok) {
  fprintf(stderr, "parsing failed, terminating the test; failing token: ");
  lispm_print_short(tok);
  fprintf(stderr, "\n");
  exit(1);
}
static void lex_error(const char *file, unsigned line) {
  fprintf(stderr, "lexing failed, terminating the test; failed around: %.12s\n", M.pc);
  exit(1);
}

int main(int argc, char *argv[]) {
  int result = 0;
  int comment = 0;
  lispm_init();
  lispm_trace_stack();
  lispm_trace.lex_error = lex_error;
  lispm_trace.parse_error = parse_error;
  for (; M.pc < M.pc_end; ++M.pc) {
    if (*M.pc == ';') comment = 1;
    if (comment) {
      if (*M.pc == '\n') comment = 0;
      continue;
    }
    if (*M.pc <= ' ') continue;
    LispmObj testname = lispm_parse_quote();
    if (!lispm_obj_is_literal(testname)) {
      fprintf(stderr, "Expected a test name literal, got: ");
      lispm_print_short(testname);
      fprintf(stderr, "\nThe layout of the test file is probably broken; terminating\n");
      return 1;
    }
    fprintf(stderr, "\n");
    M.stack_bottom_mark = lispm_rt_stack_mark();
    lispm_reset_runtime_stats();
    LispmObj actual = lispm_eval();
    lispm_print_runtime_stats();
    LispmObj expected = lispm_parse_quote();

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