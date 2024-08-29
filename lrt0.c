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

/* TODO: these probably belong to lispm.h */
static unsigned num_decode(Sym s) {
  LISPM_EVAL_CHECK(lispm_sym_is_shortnum(s) || lispm_sym_is_longnum(s), LISPM_ERR_EVAL, "expected a number, got: ", s);
  if (lispm_sym_is_shortnum(s)) return lispm_shortnum_val(s);

  Sym hi, lo, *cons;
  cons = lispm_st_obj_unpack(s), hi = cons[0], lo = cons[1];
  LISPM_ASSERT(lispm_sym_is_shortnum(lo));
  return (lispm_shortnum_val(hi) << LISPM_SHORTNUM_BITS) | lispm_shortnum_val(lo);
}
static Sym num_encode(unsigned e) {
  if (lispm_shortnum_can_represent(e)) return lispm_make_shortnum(e);
  Sym arr[2] = {lispm_make_shortnum(e >> LISPM_SHORTNUM_BITS),
                lispm_make_shortnum(e & ((1u << LISPM_SHORTNUM_BITS) - 1))};
  return lispm_st_obj_alloc(LISPM_ST_OBJ_LONGNUM, arr);
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

  unsigned res_begin = num_decode(begin);
  unsigned res_end = !lispm_sym_is_nil(end) ? num_decode(end) : src_len;
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

static Sym BAND(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  return num_encode(num_decode(p) & num_decode(q));
}
static Sym BOR(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  return num_encode(num_decode(p) | num_decode(q));
}
static Sym BXOR(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  return num_encode(num_decode(p) ^ num_decode(q));
}
static Sym BNOT(Sym a) {
  a = lispm_evquote(a);
  return num_encode(~num_decode(a));
}

LISPM_BUILTINS_EXT(LRT0_SYMS) = {{"MODULO"}};

static int arith_unpack(Sym a, unsigned *p, unsigned *q) {
  Sym *pq = lispm_cons_unpack_user(a);
  Sym *qm = lispm_cons_unpack_user(pq[1]);
  *p = num_decode(pq[0]), *q = num_decode(qm[0]);
  if (lispm_sym_is_nil(qm[1])) return 0;
  Sym m = lispm_evquote(qm[1]);
  LISPM_EVAL_CHECK(m == lispm_builtin_as_sym(LRT0_SYMS), LISPM_ERR_EVAL,
                   "only MODULO is allowed as third argument, got: ", m);
  return 1;
}
static Sym ADD(Sym a) {
  unsigned p, q, r;
  int is_modulo = arith_unpack(a, &p, &q);
  int overflow = __builtin_uadd_overflow(p, q, &r);
  LISPM_EVAL_CHECK(is_modulo || !overflow, LISPM_ERR_EVAL, "integer overflow, args: ", a);
  return num_encode(r);
}
static Sym MUL(Sym a) {
  unsigned p, q, r;
  int is_modulo = arith_unpack(a, &p, &q);
  int overflow = __builtin_umul_overflow(p, q, &r);
  LISPM_EVAL_CHECK(is_modulo || !overflow, LISPM_ERR_EVAL, "integer overflow, args: ", a);
  return num_encode(r);
}
static Sym SUB(Sym a) {
  unsigned p, q, r;
  int is_modulo = arith_unpack(a, &p, &q);
  int overflow = __builtin_usub_overflow(p, q, &r);
  LISPM_EVAL_CHECK(is_modulo || !overflow, LISPM_ERR_EVAL, "integer overflow, args: ", a);
  return num_encode(r);
}
static Sym NEG(Sym a) { return num_encode(-num_decode(lispm_evquote(a))); }

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