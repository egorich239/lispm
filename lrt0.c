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
static Sym ATOM(Sym a) { return is_atom(lispm_evquote(a)) ? SYM_T : SYM_NIL; }
static Sym EQ(Sym a) {
  Sym x, y;
  lispm_args_unpack2(a, &x, &y);
  return is_atom(x) && x == y ? SYM_T : SYM_NIL;
}

static Sym PROGRAM(Sym e) {
  EVAL_CHECK(is_nil(e), ERR_EVAL);
  return lispm_alloc_pointer(PAGE_PROGRAM, make_unsigned(0),
                             make_unsigned(lispm_page_size(PAGE_PROGRAM, 0)));
}
static Sym GETC(Sym a) {
  Sym page, offs, len, ptr = lispm_evquote(a);
  EVAL_CHECK(is_pointer(ptr), ERR_EVAL);
  lispm_pointer_unpack(ptr, &page, &offs, &len);
  if (!unsigned_val(len))
    return unsigned_val(0); /* position right after the interval */
  return make_unsigned(
      *(unsigned char *)lispm_page_loc(page, unsigned_val(offs), 1));
}
static Sym COPY(Sym a) {
  Sym dest, src, dest_offs, src_offs, dest_len, src_len;
  lispm_args_unpack2(a, &dest, &src);
  EVAL_CHECK(is_pointer(dest) && is_pointer(src), ERR_EVAL);
  lispm_pointer_unpack(dest, &dest, &dest_offs, &dest_len);
  lispm_pointer_unpack(src, &src, &src_offs, &src_len);
  struct PageDesc *dest_page = lispm_page_desc(dest);
  EVAL_CHECK(dest_page->flags == PAGE_FLAG_RW, ERR_EVAL);
  if (src_len < dest_len) dest_len = src_len;
  __builtin_memcpy(lispm_page_loc(dest, unsigned_val(dest_offs), 1),
                   lispm_page_loc(src, unsigned_val(src_offs), 1),
                   unsigned_val(dest_len));
  return dest_len;
}
static Sym STR(Sym e) {
  e = lispm_evquote(e);
  EVAL_CHECK(is_literal(e), ERR_EVAL);
  return lispm_literal_name_pointer(e);
}
static Sym CHARS(Sym a) {
  a = lispm_evquote(a);
  Sym pg, offs, len;
  EVAL_CHECK(is_pointer(a), ERR_EVAL);
  lispm_pointer_unpack(a, &pg, &offs, &len);
  const unsigned char *p = lispm_page_loc(pg, unsigned_val(offs), 1);
  unsigned l = unsigned_val(len);
  p += l;
  Sym res = SYM_NIL;
  while (l--)
    res = lispm_alloc_cons(make_unsigned(*--p), res);
  return res;
}
static Sym BAND(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  EVAL_CHECK(is_unsigned(p) && is_unsigned(q), ERR_EVAL);
  return unsigned_band(p, q);
}
static Sym BNOT(Sym a) {
  a = lispm_evquote(a);
  EVAL_CHECK(is_unsigned(a), ERR_EVAL);
  return unsigned_bnot(a);
}
static Sym ADD(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  EVAL_CHECK(is_unsigned(p) && is_unsigned(q), ERR_EVAL);
  int overflow;
  Sym r = unsigned_add(p, q, &overflow);
  EVAL_CHECK(!overflow, ERR_EVAL);
  return r;
}
static Sym MUL(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  EVAL_CHECK(is_unsigned(p) && is_unsigned(q), ERR_EVAL);
  int overflow;
  Sym r = unsigned_mul(p, q, &overflow);
  EVAL_CHECK(!overflow, ERR_EVAL);
  return r;
}
static Sym SUB(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  EVAL_CHECK(is_unsigned(p) && is_unsigned(q), ERR_EVAL);
  int overflow;
  Sym r = unsigned_sub(p, q, &overflow);
  EVAL_CHECK(!overflow, ERR_EVAL);
  return r;
}
static Sym SPAN(Sym args) {
  Sym span, start, len;
  lispm_cons_unpack_user(args, &span, &start);
  lispm_cons_unpack_user(start, &start, &len);
  EVAL_CHECK(is_pointer(span), ERR_EVAL);
  Sym page, offs, old_len;
  lispm_pointer_unpack(span, &page, &offs, &old_len);
  EVAL_CHECK(start <= old_len, ERR_EVAL);
  int o;
  len = !is_nil(len) ? lispm_evquote(len) : unsigned_sub(old_len, start, &o);
  EVAL_CHECK(len <= old_len, ERR_EVAL);
  return lispm_alloc_pointer(page, unsigned_add(offs, start, &o), len);
}
static Sym PARSE(Sym a) {
  a = lispm_evquote(a);
  EVAL_CHECK(is_pointer(a), ERR_EVAL);
  Sym pg, offs, len;
  lispm_pointer_unpack(a, &pg, &offs, &len);
  return lispm_parse(lispm_page_loc(pg, unsigned_val(offs), 1));
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