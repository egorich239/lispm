#include <liblispm/builtins.h>
#include <liblispm/debug.h>
#include <liblispm/lispm.h>
#include <liblispm/obj.h>
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

static const char *text = 0;
static LispmObj io_readline(LispmObj args) {
  if (text) free((void *)text);
  text = readline("> ");
  printf("\n");
  return lispm_return0(text ? lispm_make_shortnum(1) : LISPM_SYM_NIL);
}
static LispmObj eval(LispmObj args) {
  lispm.pc = text;
  lispm.pc_end = text + strlen(text);
  return lispm_return0(lispm_eval0());
}
static LispmObj io_print(LispmObj args) {
  lispm_print_short(args);
  printf("\n");
  return lispm_return0(LISPM_SYM_NIL);
}

LISPM_BUILTINS_EXT(IO) = {
    {"#io:readline", io_readline},
    {"#eval",        eval       },
    {"#io:print",    io_print   }
};

int main() {
  rl_bind_key('\n', handle_nl);
  rl_bind_key('\r', handle_nl);

  if (!lispm_init()) return 1;
  lispm_trace_stack();
  lispm_eval();
  return 0;
}