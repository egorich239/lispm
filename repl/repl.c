#include <liblispm/builtins.h>
#include <liblispm/debug.h>
#include <liblispm/lispm.h>
#include <liblispm/obj.h>
#include <liblispm/trace.h>
#include <liblispm/types.h>

#include <readline/readline.h>
#include <stdlib.h>
#include <string.h>

extern const char _binary_main_lispm_start[];
extern const char _binary_main_lispm_end[];

static int handle_nl(int count, int key) {
  int paren_count = 0;
  int is_comment = 0;
  int has_non_ws = 0;
  for (const char *p = rl_line_buffer; p != rl_line_buffer + rl_point; ++p) {
    if (*p == '\n') is_comment = 0;
    if (*p == ';') is_comment = 1;
    if (is_comment) continue;

    if (*p <= ' ') continue;
    has_non_ws = 1;

    if (*p == '(') paren_count++;
    if (*p == ')' && !paren_count--) return 1;
  }
  if (!paren_count && has_non_ws)
    rl_done = 1;
  else
    printf("\n");
  return 0;
}

LispmObj stack[4 * 1024 * 1024];
char strings[16 * 1024 * 1024];
unsigned htable[4 * 1024 * 1024];

struct Lispm lispm = {
    /*stack*/
    .stack = stack,
    .stack_end = stack + (sizeof(stack) / sizeof(*stack)),

    /*strings*/
    .strings = strings,
    .strings_end = strings + sizeof(strings),
    .tp = strings,

    /*program*/
    .pc = _binary_main_lispm_start,
    .pc_end = _binary_main_lispm_end,

    /*htable*/
    .htable = htable,
    .htable_end = htable + (sizeof(htable) / sizeof(*htable)),

    /**/
    .stack_depth_limit = 16384u,
};

enum { LINES_CACHE_SIZE = 2 };
static const char *lines_cache[LINES_CACHE_SIZE];

static LispmObj eval(LispmObj args);
static LispmObj io_readline(LispmObj args);
static LispmObj io_line_free(LispmObj args);
static LispmObj io_print(LispmObj args);

LISPM_BUILTINS_EXT(IO) = {
    {"#:io-line", 0, io_line_free, LISPM_BUILTIN_TYPETAG},
    {"#io:readline", io_readline},
    {"#eval", eval},
    {"#io:print", io_print},
};

static LispmObj io_readline(LispmObj args) {
  int c = 0;
  for (; c < LINES_CACHE_SIZE; ++c)
    if (!lines_cache[c]) break;
  LISPM_EVAL_CHECK(c != LINES_CACHE_SIZE, args, panic, "lines cache memory exhausted: ", args);
  lines_cache[c] = readline("> ");
  printf("\n");
  return lispm_return0(lines_cache[c] ? lispm_cons_alloc(lispm_make_shortnum(c), lispm_builtin_value(IO + 0))
                                      : LISPM_SYM_NIL);
}
static LispmObj io_line_free(LispmObj args) {
  LispmObj *o = lispm_obj_unpack(args);
  LISPM_ASSERT(o[0] == lispm_builtin_value(IO + 0));
  unsigned offs = lispm_shortnum_val(o[1]);
  fprintf(stderr, "free'd the line: %s\n", lines_cache[offs]);
  free((void *)lines_cache[offs]);
  lines_cache[offs] = 0;
  return lispm_return0(LISPM_SYM_NIL);
}
static LispmObj eval(LispmObj args) {
  LispmObj line;
  LISPM_EVAL_CHECK(lispm_list_scan(&line, args, 1) == 1, args, panic, "expected one line argument, got: ", args);
  LISPM_EVAL_CHECK(lispm_obj_is_cons(line) && lispm_obj_unpack(line)[0] == lispm_builtin_value(IO + 0), args, panic,
                   "expected one line argument, got: ", args);
  lispm.pc = lines_cache[lispm_shortnum_val(lispm_obj_unpack(line)[1])];
  lispm.pc_end = lispm.pc + strlen(lispm.pc);
  return lispm_return0(lispm_eval0());
}
static LispmObj io_print(LispmObj args) {
  lispm_print_short(args);
  printf("\n");
  return lispm_return0(LISPM_SYM_NIL);
}

int main() {
  rl_bind_key('\n', handle_nl);
  rl_bind_key('\r', handle_nl);

  if (!lispm_init()) return 1;
  lispm_trace_stack();
  lispm_eval();
  return 0;
}