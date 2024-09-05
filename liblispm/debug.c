#include <liblispm/debug.h>

#include <liblispm/builtins.h>
#include <liblispm/lispm.h>
#include <liblispm/obj.h>
#include <liblispm/trace.h>
#include <liblispm/types.h>

#include <stdio.h>

/**/
#include <liblispm/interal-macros.h>

extern const struct LispmBuiltin lispm_builtins_start[];

#if LISPM_CONFIG_VERBOSE
struct LispmTraceCallbacks lispm_trace = {};
struct CallFrame {
  Obj fn;
  Obj resolved;
  Obj args;
};

enum { STACK_TRACE_DEPTH = 32 };
static struct CallFrame stack_trace[STACK_TRACE_DEPTH];
static unsigned stack_trace_depth;
#endif

static const char *literal_name(Obj l) {
  LISPM_ASSERT(lispm_obj_is_literal(l));
  return lispm.strings + (lispm.htable[lispm_literal_ht_offs(l)] >> 2);
}

void lispm_print_short(Obj sym) {
  if (lispm_obj_is_nil(sym)) {
    fprintf(stderr, "()");
    return;
  }
  if (lispm_obj_is_literal(sym)) {
    fprintf(stderr, "%s", literal_name(sym));
    return;
  }
  if (lispm_obj_is_shortnum(sym)) {
    fprintf(stderr, "%u", lispm_shortnum_val(sym));
    return;
  }
  if (lispm_obj_is_builtin_sym(sym)) {
    fprintf(stderr, "<builtin %s>", lispm_builtins_start[lispm_builtin_sym_offs(sym)].name);
    return;
  }
  if (lispm_obj_is_special(sym)) {
    fprintf(stderr, "<special %x>", sym);
    return;
  }
  if (lispm_obj_is_cons(sym)) {
    enum { COUNTER_INIT_VALUE = 7 };
    int counter = COUNTER_INIT_VALUE;
    fprintf(stderr, "(");
    while (lispm_obj_is_cons(sym)) {
      Obj *cons = lispm_obj_unpack(sym);
      sym = cons[0];
      if (counter < 0) continue;
      if (counter == 0) {
        fprintf(stderr, " ...");
        --counter;
        continue;
      }
      if (counter-- < COUNTER_INIT_VALUE) { fprintf(stderr, " "); }
      lispm_print_short(cons[1]);
    }
    if (!lispm_obj_is_nil(sym) && counter >= 0) {
      fprintf(stderr, " ");
      lispm_print_short(sym);
    }
    fprintf(stderr, lispm_obj_is_nil(sym) ? ")" : "]");
  }
  if (lispm_obj_is_st_obj(sym)) {
    Obj *cab = lispm_obj_unpack(sym);
    fprintf(stderr, "[");
    for (int i = 0; i < lispm_st_obj_st_size(sym); ++i) {
      if (i) fprintf(stderr, " ");
      lispm_print_short(cab[i]);
    }
    fprintf(stderr, "]");
    return;
  }
}

#if LISPM_CONFIG_VERBOSE
static int stack_depths[2];
static void trace_stack_depth(enum LispmTraceStack stack, int depth) {
  if (stack_depths[stack] < depth) stack_depths[stack] = depth;
}
static void trace_assertion(const char *file, unsigned line, const char *msg) {
  fprintf(stderr, "%s:%u: ASSERT: %s\n", file, line, msg);
}
static void trace_panic(const char *file, unsigned line, const char *msg, Obj ctx) {
  fprintf(stderr, "%s:%u: %s", file, line, msg);
  lispm_print_short(ctx);
  fprintf(stderr, "\n");
}
static void trace_illegal_bind(const char *file, unsigned line, Obj sym) {
  fprintf(stderr, "it is fobidden to bind to: ");
  lispm_print_short(sym);
  fprintf(stderr, "\n");
}
static void trace_unbound_symbol(const char *file, unsigned line, Obj sym) {
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
  if (lispm_obj_is_triplet(frame.resolved)) {
    Obj *cap = lispm_obj_unpack(frame.resolved);
    fprintf(stderr, "    ");
    lispm_print_short(cap[0]);
    fprintf(stderr, "\n");
  }
  fprintf(stderr, "\n");
}

static void trace_full_apply_enter(Obj f, Obj resolved, Obj a) {
  print_call_frame((struct CallFrame){.fn = f, .resolved = resolved, .args = a});
}

static void trace_apply_enter(Obj f, Obj resolved, Obj a) {
  const unsigned target = stack_trace_depth < STACK_TRACE_DEPTH ? stack_trace_depth : STACK_TRACE_DEPTH - 1;
  stack_trace[target] = (struct CallFrame){.fn = f, .resolved = resolved, .args = a};
  ++stack_trace_depth;
}

static void trace_apply_leave() { --stack_trace_depth; }
#endif

void lispm_trace_full(void) {
#if LISPM_CONFIG_VERBOSE
  lispm_trace.apply_enter = trace_full_apply_enter;
  lispm_trace.stack_depth = trace_stack_depth;
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
  lispm_trace.stack_depth = trace_stack_depth;
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

void lispm_reset_runtime_stats(void) {
#if LISPM_CONFIG_VERBOSE
  stack_depths[0] = stack_depths[1] = 0;
#endif
}
void lispm_print_runtime_stats(void) {
#if LISPM_CONFIG_VERBOSE
  fprintf(stderr, "=== Runtime statistics ===\n");
  fprintf(stderr, "  native stack depth (max): %i\n", stack_depths[LISPM_TRACE_STACK_NATIVE]);
  fprintf(stderr, "  object stack depth (max): %i\n", stack_depths[LISPM_TRACE_STACK_OBJECTS]);
#endif
}

void lispm_dump(Obj sym) {
  static int indent = 0;
  static int same_line = 0;

  const unsigned *stack = lispm.stack;

  if (!same_line)
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
  same_line = 0;

  if (lispm_obj_is_cons(sym)) {
    fprintf(stderr, "(");
    indent += 2;
    same_line = 1;
    while (lispm_obj_is_cons(sym)) {
      Obj car = stack[lispm_st_obj_st_offs(sym) + 1];
      sym = stack[lispm_st_obj_st_offs(sym) + 0];
      lispm_dump(car);
    }
    if (sym != LISPM_SYM_NIL) lispm_dump(sym);

    indent -= 2;
    for (int i = 0; i < indent; ++i)
      fprintf(stderr, " ");
    fprintf(stderr, sym != LISPM_SYM_NIL ? "]\n" : ")\n");
  } else if (lispm_obj_is_triplet(sym)) {
    unsigned offs = lispm_st_obj_st_offs(sym);
    Obj cap = stack[offs], par = stack[offs + 1], body = stack[offs + 2];
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
