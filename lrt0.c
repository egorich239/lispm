#include "lispm.h"
#include "sym.h"

static Sym CONS(Sym a) {
  Sym x, y;
  lispm_args_unpack2(a, &x, &y);
  return lispm_alloc_cons(x, y);
}
static Sym CAR(Sym a) {
  Sym car, cdr;
  lispm_cons_unpack_user(lispm_evquote(a), &car, &cdr);
  return car;
}
static Sym CDR(Sym a) {
  Sym car, cdr;
  lispm_cons_unpack_user(lispm_evquote(a), &car, &cdr);
  return cdr;
}
static Sym ATOM(Sym a) {
  return lispm_sym_is_atom(lispm_evquote(a)) ? LISPM_SYM_T : LISPM_SYM_NIL;
}
static Sym EQ(Sym a) {
  Sym x, y;
  lispm_args_unpack2(a, &x, &y);
  return lispm_sym_is_atom(x) && x == y ? LISPM_SYM_T : LISPM_SYM_NIL;
}

static Sym PROGRAM(Sym e) {
  EVAL_CHECK(lispm_sym_is_nil(e), LISPM_ERR_EVAL);
  return lispm_alloc_span(
      LISPM_PAGE_PROGRAM, lispm_make_unsigned(0),
      lispm_make_unsigned(lispm_page_size(LISPM_PAGE_PROGRAM, 0)));
}
static Sym GETC(Sym a) {
  Sym page, offs, len, ptr = lispm_evquote(a);
  EVAL_CHECK(lispm_sym_is_span(ptr), LISPM_ERR_EVAL);
  lispm_span_unpack(ptr, &page, &offs, &len);
  if (!lispm_unsigned_val(len))
    return lispm_unsigned_val(0); /* position right after the interval */
  return lispm_make_unsigned(
      *(unsigned char *)lispm_page_loc(page, lispm_unsigned_val(offs), 1));
}
static Sym COPY(Sym a) {
  Sym dest, src, dest_offs, src_offs, dest_len, src_len;
  lispm_args_unpack2(a, &dest, &src);
  EVAL_CHECK(lispm_sym_is_span(dest) && lispm_sym_is_span(src), LISPM_ERR_EVAL);
  lispm_span_unpack(dest, &dest, &dest_offs, &dest_len);
  lispm_span_unpack(src, &src, &src_offs, &src_len);
  struct PageDesc *dest_page = lispm_page_desc(dest);
  EVAL_CHECK(dest_page->flags == PAGE_FLAG_RW, LISPM_ERR_EVAL);
  if (src_len < dest_len) dest_len = src_len;
  __builtin_memcpy(lispm_page_loc(dest, lispm_unsigned_val(dest_offs), 1),
                   lispm_page_loc(src, lispm_unsigned_val(src_offs), 1),
                   lispm_unsigned_val(dest_len));
  return dest_len;
}
static Sym STR(Sym e) {
  e = lispm_evquote(e);
  EVAL_CHECK(lispm_sym_is_literal(e), LISPM_ERR_EVAL);
  return lispm_literal_name_span(e);
}
static Sym CHARS(Sym a) {
  a = lispm_evquote(a);
  Sym pg, offs, len;
  EVAL_CHECK(lispm_sym_is_span(a), LISPM_ERR_EVAL);
  lispm_span_unpack(a, &pg, &offs, &len);
  const unsigned char *p = lispm_page_loc(pg, lispm_unsigned_val(offs), 1);
  unsigned l = lispm_unsigned_val(len);
  p += l;
  Sym res = LISPM_SYM_NIL;
  while (l--)
    res = lispm_alloc_cons(lispm_make_unsigned(*--p), res);
  return res;
}
static Sym BAND(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  EVAL_CHECK(lispm_sym_is_unsigned(p) && lispm_sym_is_unsigned(q),
             LISPM_ERR_EVAL);
  return lispm_unsigned_band(p, q);
}
static Sym BNOT(Sym a) {
  a = lispm_evquote(a);
  EVAL_CHECK(lispm_sym_is_unsigned(a), LISPM_ERR_EVAL);
  return lispm_unsigned_bnot(a);
}
static Sym ADD(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  EVAL_CHECK(lispm_sym_is_unsigned(p) && lispm_sym_is_unsigned(q),
             LISPM_ERR_EVAL);
  int overflow;
  Sym r = lispm_unsigned_add(p, q, &overflow);
  EVAL_CHECK(!overflow, LISPM_ERR_EVAL);
  return r;
}
static Sym MUL(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  EVAL_CHECK(lispm_sym_is_unsigned(p) && lispm_sym_is_unsigned(q),
             LISPM_ERR_EVAL);
  int overflow;
  Sym r = lispm_unsigned_mul(p, q, &overflow);
  EVAL_CHECK(!overflow, LISPM_ERR_EVAL);
  return r;
}
static Sym SUB(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  EVAL_CHECK(lispm_sym_is_unsigned(p) && lispm_sym_is_unsigned(q),
             LISPM_ERR_EVAL);
  int overflow;
  Sym r = lispm_unsigned_sub(p, q, &overflow);
  EVAL_CHECK(!overflow, LISPM_ERR_EVAL);
  return r;
}
static Sym SPAN(Sym args) {
  Sym span, start, len;
  lispm_cons_unpack_user(args, &span, &start);
  lispm_cons_unpack_user(start, &start, &len);
  EVAL_CHECK(lispm_sym_is_span(span), LISPM_ERR_EVAL);
  Sym page, offs, old_len;
  lispm_span_unpack(span, &page, &offs, &old_len);
  EVAL_CHECK(start <= old_len, LISPM_ERR_EVAL);
  int o;
  len = !lispm_sym_is_nil(len) ? lispm_evquote(len)
                               : lispm_unsigned_sub(old_len, start, &o);
  EVAL_CHECK(len <= old_len, LISPM_ERR_EVAL);
  return lispm_alloc_span(page, lispm_unsigned_add(offs, start, &o), len);
}
static Sym PARSE(Sym a) {
  a = lispm_evquote(a);
  EVAL_CHECK(lispm_sym_is_span(a), LISPM_ERR_EVAL);
  Sym pg, offs, len;
  lispm_span_unpack(a, &pg, &offs, &len);
  return lispm_parse(lispm_page_loc(pg, lispm_unsigned_val(offs), 1));
}

struct Builtin LRT0[] = {
    {"CONS", CONS},   {"CAR", CAR},
    {"CDR", CDR},     {"ATOM", ATOM},
    {"EQ", EQ},       {"PROGRAM", PROGRAM},
    {"SPAN", SPAN},   {"STR", STR, lispm_evcap_quote},
    {"CHARS", CHARS}, {"GETC", GETC},
    {"COPY", COPY},   {"ADD", ADD},
    {"MUL", MUL},     {"SUB", SUB},
    {"BAND", BAND},   {"BNOT", BNOT},
    {"PARSE", PARSE}, {},
};