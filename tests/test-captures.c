#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include "debug.h"
#include "lispm.h"

extern struct Builtin lispm_builtins[];

extern const char _binary_test_captures_tests_txt_start[];
extern const char _binary_test_captures_tests_txt_end[];

Sym stack[4 * 1024 * 1024];
char strings[16 * 1024 * 1024];
unsigned htable[4 * 1024 * 1024];

static int sym_equal(Sym exp, Sym act) {
  if (lispm_sym_is_atom(act) && lispm_sym_is_atom(exp)) return exp == act;
  if (lispm_sym_is_error(act) && lispm_sym_is_error(exp)) return exp == act;
  if (lispm_sym_is_cons(act) && lispm_sym_is_cons(exp)) {
    Sym *aa = lispm_st_obj_unpack(exp), *bb = lispm_st_obj_unpack(act);
    if (!sym_equal(aa[0], bb[0])) return 0;
    return sym_equal(aa[1], bb[1]);
  }
  if (lispm_sym_is_lambda(act) && lispm_sym_is_cons(exp)) {
    Sym *aa = lispm_st_obj_unpack(exp), *bb = lispm_st_obj_unpack(act);
    Sym parms, body;
    aa = lispm_st_obj_unpack(aa[1]), parms = aa[0], *aa = aa[1];
    aa = lispm_st_obj_unpack(aa[1]), body = aa[0];
    if (!sym_equal(parms, bb[1])) return 0;
    return sym_equal(body, bb[2]);
  }

  return 0;
}

static int sym_equal_capture(Sym expected, Sym actual) {
  Sym *cons, exp, name_exp, value_exp, act, name_act, value_act;
  /* VM stores catpures in pairs, not in lists of two elements,
     whereas expectations are parsed as lists of two elements */
  while (lispm_sym_is_cons(expected) && lispm_sym_is_cons(actual)) {
    cons = lispm_st_obj_unpack(expected), exp = cons[0], expected = cons[1];
    cons = lispm_st_obj_unpack(exp), name_exp = cons[0], value_exp = cons[1];
    cons = lispm_st_obj_unpack(value_exp), value_exp = cons[0];
    cons = lispm_st_obj_unpack(actual), act = cons[0], actual = cons[1];
    cons = lispm_st_obj_unpack(act), name_act = cons[0], value_act = cons[1];
    if (!sym_equal(name_exp, name_act) || !sym_equal(value_exp, value_act)) return 0;
  }
  if (!lispm_sym_is_nil(expected) || !lispm_sym_is_nil(actual)) return 0;
  return 1;
}

static int failure;
static Sym next_lambda;
static void trace_lambda(Sym lambda) {
  lispm_print_short(lambda);
  fprintf(stderr, "\n");
  if (lispm_sym_is_nil(next_lambda)) {
    fprintf(stderr, "no more captures are expected, but got another lambda definition: ");
    lispm_print_short(lambda);
    fprintf(stderr, "\n");
    failure = 1;
    return;
  }
  Sym act   = lispm_st_obj_unpack(lambda)[0];
  Sym *cons = lispm_st_obj_unpack(next_lambda), exp = cons[0];
  next_lambda = cons[1];
  if (!sym_equal_capture(exp, act)) {
    fprintf(stderr, "  expected ");
    lispm_print_short(exp);
    fprintf(stderr, "\n");
    fprintf(stderr, "    actual ");
    lispm_print_short(act);
    fprintf(stderr, "\n");
    failure = 1;
  }
}

struct Lispm lispm = {
    .builtins = lispm_builtins,

    /*stack*/
    .stack = stack,
    .sp    = stack + (sizeof(stack) / sizeof(*stack)),
    .pp    = stack + LISPM_PP_OFFSET,

    /*strings*/
    .strings     = strings,
    .strings_end = strings + sizeof(strings),
    .tp          = strings,

    /*program*/
    .program     = _binary_test_captures_tests_txt_start,
    .program_end = _binary_test_captures_tests_txt_end,
    .pc          = _binary_test_captures_tests_txt_start,

    /*htable*/
    .htable     = htable,
    .htable_end = htable + (sizeof(htable) / sizeof(*htable)),
};

#define M lispm

/* we have to wrap parse calls into try block, otherwise lex/parse error segfault */
static Sym parse_result;
static void parse_void(void) {
  parse_result = LISPM_ERR_PARSE;
  parse_result = lispm_parse(M.pc, M.program_end);
}

int main(int argc, char *argv[]) {
  int comment = 0;
  failure     = 0;
  lispm_init();
  lispm_trace();
  M.trace.lambda_cons = trace_lambda;
  for (; M.pc < M.program_end; ++M.pc) {
    if (*M.pc == ';') comment = 1;
    if (comment) {
      if (*M.pc == '\n') comment = 0;
      continue;
    }
    if (*M.pc <= ' ') continue;
    M.sp        = M.stack + (sizeof(stack) / sizeof(*stack));
    next_lambda = LISPM_SYM_NIL;
    lispm_rt_try(parse_void);
    next_lambda = parse_result;
    Sym actual  = lispm_exec();
    Sym expected;
    lispm_rt_try(parse_void);
    expected = parse_result;

    if (next_lambda == LISPM_ERR_PARSE || actual == LISPM_ERR_PARSE || expected == LISPM_ERR_PARSE) {
      fprintf(stderr, "Lexing or parsing error encountered; terminating\n");
      return 1;
    }
    if (!sym_equal(actual, expected)) {
      fprintf(stderr, "  expected result: ");
      lispm_print_short(expected);
      fprintf(stderr, "\n    actual result: ");
      lispm_print_short(actual);
      fprintf(stderr, "\n");
      failure = 1;
    }
  }
  return failure;
}