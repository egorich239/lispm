#include "lrt0.h"
#include "lispm.h"

#define M lispm

enum {
  PAGE_PROGRAM = 0,
  PAGE_STRINGS = 1,
};

static void check_access_read(unsigned access, Sym ptr) {
  LISPM_EVAL_CHECK(access & PAGE_ACCESS_R, LISPM_ERR_EVAL, "read access denied: ", ptr);
}
static void check_access_write(unsigned access, Sym ptr) {
  LISPM_EVAL_CHECK(access & PAGE_ACCESS_W, LISPM_ERR_EVAL, "write access denied: ", ptr);
}
static Sym lispm_literal_name_span(Sym s) {
  LISPM_ASSERT(lispm_sym_is_literal(s));
  unsigned offs = lispm_literal_str_offs(s);
  unsigned len = __builtin_strlen(M.strings + offs);
  return lispm_span_alloc(lispm_make_span(PAGE_STRINGS, offs, len));
}
static void *lispm_span_base(struct Span span, unsigned *len, unsigned *access) {
  void *ptr, *b, *e;
  unsigned page = lispm_shortnum_val(span.page);
  *len = lispm_shortnum_val(span.len);
  *access = PAGE_ACCESS_R;
  switch (page) {
  case PAGE_PROGRAM:
    b = (void *)M.program, e = (void *)M.program_end;
    break;
  case PAGE_STRINGS:
    b = M.strings, e = M.strings_end;
    break;
  default:
    lispm_rt_page(page, &b, &e, access);
  }
  ptr = b + lispm_shortnum_val(span.offs);
  LISPM_ASSERT(ptr <= e && ptr + *len <= e);
  return ptr;
}
static inline struct Span lispm_span_unpack_user(Sym span) {
  LISPM_EVAL_CHECK(lispm_sym_is_span(span), LISPM_ERR_EVAL, "span expected, got: ", span);
  return lispm_span_unpack(span);
}

static Sym PROGRAM(Sym e) {
  LISPM_EVAL_CHECK(lispm_sym_is_nil(e), LISPM_ERR_EVAL, "no arguments expected, got: ", e);
  return lispm_span_alloc(lispm_make_span(PAGE_PROGRAM, 0, M.program_end - M.program));
}
static Sym GETC(Sym ptr) {
  ptr = lispm_evquote(ptr);
  unsigned len, access;
  struct Span span = lispm_span_unpack_user(ptr);
  const unsigned char *c = lispm_span_base(span, &len, &access);
  check_access_read(access, ptr);
  if (!len) return LISPM_SYM_NIL; /* position right after the interval */
  return lispm_make_shortnum(*c);
}
static Sym COPY(Sym a) {
  Sym d, s;
  lispm_args_unpack2(a, &d, &s);
  struct Span dest = lispm_span_unpack_user(d), src = lispm_span_unpack_user(s);
  unsigned dest_len, dest_access, src_len, src_access;
  void *dest_ptr = lispm_span_base(dest, &dest_len, &dest_access),
       *src_ptr = lispm_span_base(src, &src_len, &src_access);
  LISPM_EVAL_CHECK(src_len <= dest_len, LISPM_ERR_EVAL, "copy out of bounds for args: ", a);
  check_access_read(src_access, s);
  check_access_write(dest_access, d);
  __builtin_memcpy(dest_ptr, src_ptr, src_len);
  dest.len = lispm_make_shortnum(src_len);
  return lispm_span_alloc(dest);
}
static Sym STR(Sym e) {
  e = lispm_evquote(e);
  LISPM_EVAL_CHECK(lispm_sym_is_literal(e), LISPM_ERR_EVAL, "literal expected, got: ", e);
  return lispm_literal_name_span(e);
}
static Sym CHARS(Sym ptr) {
  ptr = lispm_evquote(ptr);
  unsigned len, access;
  const unsigned char *base = lispm_span_base(lispm_span_unpack_user(ptr), &len, &access);
  check_access_read(access, ptr);
  Sym res = LISPM_SYM_NIL;
  for (const unsigned char *p = base + len; p != base;)
    res = lispm_cons_alloc(lispm_make_shortnum(*--p), res);
  return res;
}
static Sym SPAN(Sym args) {
  Sym span, begin, end, *cons;
  cons = lispm_cons_unpack_user(args), span = cons[0], begin = cons[1];
  cons = lispm_cons_unpack_user(begin), begin = cons[0], end = cons[1];
  if (!lispm_sym_is_nil(end)) end = lispm_evquote(end);
  struct Span res = lispm_span_unpack_user(span);
  unsigned src_offs = lispm_shortnum_val(res.offs);
  unsigned src_len = lispm_shortnum_val(res.len);

  unsigned res_begin = lispm_shortnum_val(begin);
  unsigned res_end = !lispm_sym_is_nil(end) ? lispm_shortnum_sval(end) : src_len;
  if (((int)res_end) < 0) res_end = src_len + res_end;
  LISPM_EVAL_CHECK(res_begin <= res_end && res_end <= src_len, LISPM_ERR_EVAL, "subspan does not fit: ", span);

  /* we rely on the fact that all LISP-visible span constructors allocate shortnum-pages,
     and all operations can only shrink their input spans,
     so here we do not eval-check that the resulting values fit into shortnum,
     although we do indirectly assert-check.
  */
  res.offs = lispm_make_shortnum(src_offs + res_begin);
  res.len = lispm_make_shortnum(res_end - res_begin);
  return lispm_span_alloc(res);
}
static void lispm_program_get(Sym ptr, const char **begin, const char **end) {
  ptr = lispm_evquote(ptr);
  const struct Span span = lispm_span_unpack_user(ptr);
  LISPM_EVAL_CHECK(lispm_shortnum_val(span.page) == PAGE_PROGRAM, LISPM_ERR_EVAL,
                   "parsing non-program page is prohibited, got: ", ptr);
  unsigned access, len;
  *begin = lispm_span_base(span, &len, &access);
  *end = *begin + len;
}
static Sym PARSE(Sym ptr) {
  const char *begin, *end;
  lispm_program_get(ptr, &begin, &end);
  return lispm_parse(begin, end);
}
static Sym IMPORT(Sym ptr) {
  /* the important difference from eval, is that parsing happens in-place,
     and the result of the parsing is evaluated immediately,
     which avoids late binding issues */
  const char *begin, *end;
  lispm_program_get(ptr, &begin, &end);
  return lispm_eval(begin, end);
}

LISPM_BUILTINS_EXT(LRT0_SYMS) = {{"MODULO"}};

typedef enum { OP_OR, OP_AND, OP_XOR, OP_ADD, OP_SUB, OP_MUL } BinOp;
static int binop_unpack(Sym a, int arith, unsigned *p, unsigned *q) {
  Sym *pq = lispm_cons_unpack_user(a);
  Sym *qm = lispm_cons_unpack_user(pq[1]);
  *p = pq[0], *q = qm[0];
  if (lispm_sym_is_nil(qm[1])) return 0;
  LISPM_EVAL_CHECK(arith, LISPM_ERR_EVAL, "exactly two operands expected, got: ", a);

  Sym m = lispm_evquote(qm[1]);
  LISPM_EVAL_CHECK(m == lispm_builtin_as_sym(LRT0_SYMS), LISPM_ERR_EVAL,
                   "only MODULO is allowed as third argument, got: ", m);
  return 1;
}
static Sym binop(Sym args, BinOp op, int arith) {
  Sym p, q;
  int mod2 = binop_unpack(args, arith, &p, &q);
  LISPM_EVAL_CHECK(lispm_sym_is_shortnum(p) && lispm_sym_is_shortnum(q), LISPM_ERR_EVAL,
                   "both operands must be numeric, got: ", args);
  int overflow;
  Sym res;
  switch (op) {
  case OP_OR:
    return lispm_shortnum_bitwise_or(p, q);
  case OP_AND:
    return lispm_shortnum_bitwise_and(p, q);
  case OP_XOR:
    return lispm_shortnum_bitwise_xor(p, q);
  case OP_ADD:
    res = lispm_shortnum_add(p, q, &overflow);
    break;
  case OP_SUB:
    res = lispm_shortnum_sub(p, q, &overflow);
    break;
  case OP_MUL:
    res = lispm_shortnum_mul(p, q, &overflow);
    break;
  }
  LISPM_EVAL_CHECK(mod2 || !overflow, LISPM_ERR_EVAL, "integer overflow, args: ", args);
  return res;
}

static Sym BAND(Sym a) { return binop(a, OP_AND, 0); }
static Sym BOR(Sym a) { return binop(a, OP_OR, 0); }
static Sym BXOR(Sym a) { return binop(a, OP_XOR, 0); }
static Sym ADD(Sym a) { return binop(a, OP_ADD, 1); }
static Sym SUB(Sym a) { return binop(a, OP_SUB, 1); }
static Sym MUL(Sym a) { return binop(a, OP_MUL, 1); }

static Sym BNOT(Sym val) {
  val = lispm_evquote(val);
  LISPM_EVAL_CHECK(lispm_sym_is_shortnum(val), LISPM_ERR_EVAL, "numeric value expected, got: ", val);
  return lispm_shortnum_bitwise_not(val);
}
static Sym NEG(Sym val) {
  val = lispm_evquote(val);
  LISPM_EVAL_CHECK(lispm_sym_is_shortnum(val), LISPM_ERR_EVAL, "numeric value expected, got: ", val);
  return lispm_shortnum_neg(val);
}

LISPM_BUILTINS_EXT(LRT0) = {
    {"program", PROGRAM},
    {"import", IMPORT},
    {"parse", PARSE},
    {"span", SPAN},
    {"str", STR, lispm_evcap_quote},
    {"chars", CHARS},
    {"getc", GETC},
    {"copy", COPY},
    {"+", ADD},
    {"*", MUL},
    {"-", SUB},
    {"~", NEG},
    {"bitwise-and", BAND},
    {"bitwise-or", BOR},
    {"bitwise-xor", BXOR},
    {"bitwise-not", BNOT},
};