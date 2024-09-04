#include "debug.h"
#include "lispm-builtins.h"
#include "lispm.h"
#include "lrt0.h"

#include <stdio.h>

extern const struct Builtin lispm_builtins_start[];

#if LISPM_CONFIG_VERBOSE
struct LispmTraceCallbacks lispm_trace = {};
struct CallFrame {
  Sym fn;
  Sym resolved;
  Sym args;
};

enum { STACK_TRACE_DEPTH = 32 };
static struct CallFrame stack_trace[STACK_TRACE_DEPTH];
static unsigned stack_trace_depth;
#endif

static const char *literal_name(Sym l) { return lispm.strings + lispm_literal_str_offs(l); }

void lispm_print_short(Sym sym) {
  if (lispm_sym_is_nil(sym)) {
    fprintf(stderr, "()");
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
  if (lispm_sym_is_builtin_sym(sym)) {
    fprintf(stderr, "<builtin %s>", lispm_builtins_start[lispm_builtin_sym_offs(sym)].name);
    return;
  }
  if (lispm_sym_is_special(sym)) {
    fprintf(stderr, "<special %x>", sym);
    return;
  }
  if (lispm_sym_is_triplet(sym)) {
    Sym *cab = lispm_st_obj_unpack(sym);
    fprintf(stderr, "(lambda ");
    lispm_print_short(cab[1]);
    fprintf(stderr, " ");
    lispm_print_short(cab[2]);
    fprintf(stderr, ")");
    return;
  }
  LISPM_ASSERT(lispm_sym_is_cons(sym));
  enum { COUNTER_INIT_VALUE = 7 };
  int counter = COUNTER_INIT_VALUE;
  fprintf(stderr, "(");
  while (lispm_sym_is_cons(sym)) {
    Sym *cons = lispm_st_obj_unpack(sym);
    sym = cons[1];
    if (counter < 0) continue;
    if (counter == 0) {
      fprintf(stderr, " ...");
      --counter;
      continue;
    }
    if (counter-- < COUNTER_INIT_VALUE) { fprintf(stderr, " "); }
    lispm_print_short(cons[0]);
  }
  if (!lispm_sym_is_nil(sym) && counter >= 0) {
    fprintf(stderr, " ");
    lispm_print_short(sym);
  }
  fprintf(stderr, lispm_sym_is_nil(sym) ? ")" : "]");
}

#if LISPM_CONFIG_VERBOSE
static void trace_assertion(const char *file, unsigned line, const char *msg) {
  fprintf(stderr, "%s:%u: ASSERT: %s\n", file, line, msg);
}
static void trace_panic(const char *file, unsigned line, const char *msg, Sym ctx) {
  fprintf(stderr, "%s:%u: %s", file, line, msg);
  lispm_print_short(ctx);
  fprintf(stderr, "\n");
}
static void trace_illegal_bind(const char *file, unsigned line, Sym sym) {
  fprintf(stderr, "it is fobidden to bind to: ");
  lispm_print_short(sym);
  fprintf(stderr, "\n");
}
static void trace_unbound_symbol(const char *file, unsigned line, Sym sym) {
  fprintf(stderr, "unbound symbol: ");
  lispm_print_short(sym);
  fprintf(stderr, "\n");
}
static void print_call_frame(struct CallFrame frame) {
  fprintf(stderr, "  ");
  lispm_print_short(frame.fn);
  fprintf(stderr, " = ");
  lispm_print_short(frame.resolved);
  fprintf(stderr, ": ");
  lispm_print_short(frame.args);
  fprintf(stderr, "\n");
  if (lispm_sym_is_triplet(frame.resolved)) {
    Sym *cap = lispm_st_obj_unpack(frame.resolved);
    fprintf(stderr, "    ");
    lispm_print_short(cap[0]);
    fprintf(stderr, "\n");
  }
  fprintf(stderr, "\n");
}

static void trace_full_apply_enter(Sym f, Sym resolved, Sym a) {
  print_call_frame((struct CallFrame){.fn = f, .resolved = resolved, .args = a});
}

static void trace_apply_enter(Sym f, Sym resolved, Sym a) {
  const unsigned target = stack_trace_depth < STACK_TRACE_DEPTH ? stack_trace_depth : STACK_TRACE_DEPTH - 1;
  stack_trace[target] = (struct CallFrame){.fn = f, .resolved = resolved, .args = a};
  ++stack_trace_depth;
}

static void trace_apply_leave() { --stack_trace_depth; }
#endif

void lispm_trace_full(void) {
#if LISPM_CONFIG_VERBOSE
  lispm_trace.apply_enter = trace_full_apply_enter;
  lispm_trace.panic = trace_panic;
  lispm_trace.assertion = trace_assertion;
  lispm_trace.illegal_bind = trace_illegal_bind;
  lispm_trace.unbound_symbol = trace_unbound_symbol;
#endif
}

void lispm_trace_stack(void) {
#if LISPM_CONFIG_VERBOSE
  lispm_trace.apply_enter = trace_apply_enter;
  lispm_trace.apply_leave = trace_apply_leave;
  lispm_trace.panic = trace_panic;
  lispm_trace.assertion = trace_assertion;
  lispm_trace.illegal_bind = trace_illegal_bind;
  lispm_trace.unbound_symbol = trace_unbound_symbol;
#endif
}

void lispm_print_stack_trace(void) {
#if LISPM_CONFIG_VERBOSE
  if (stack_trace_depth >= STACK_TRACE_DEPTH) {
    print_call_frame(stack_trace[STACK_TRACE_DEPTH - 1]);
    fprintf(stderr, "  ... %u frames omitted\n", stack_trace_depth - STACK_TRACE_DEPTH);
    stack_trace_depth = STACK_TRACE_DEPTH - 1;
  }
  while (stack_trace_depth) {
    print_call_frame(stack_trace[--stack_trace_depth]);
  }
#endif
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
  } else if (lispm_sym_is_triplet(sym)) {
    unsigned offs = lispm_st_obj_st_offs(sym);
    Sym cap = stack[offs], par = stack[offs + 1], body = stack[offs + 2];
    fprintf(stderr, "(lambda\n");
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
