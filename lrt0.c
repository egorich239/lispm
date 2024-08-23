#include "lispm.h"
#include "sym.h"

static Sym PROGRAM(Sym e) {
  EVAL_CHECK(is_nil(e), ERR_EVAL);
  return lispm_alloc_pointer(PAGE_PROGRAM, make_unsigned(0),
                             make_unsigned(lispm_page_size(PAGE_PROGRAM, 0)));
}
static Sym GETC(Sym a) {
  Sym page, offs, len, ptr = lispm_evquote(a);
  EVAL_CHECK(is_pointer(ptr), ERR_EVAL);
  lispm_pointer_unpack(ptr, &page, &offs, &len);
  return make_unsigned(*(unsigned char *)lispm_page_loc(page, offs, 1));
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
static Sym SPAN(Sym a) {
  Sym p, b, l, e;
  lispm_cons_unpack_user(a, &p, &b);
  lispm_cons_unpack_user(b, &b, &l);
  EVAL_CHECK(is_pointer(p), ERR_EVAL);
  Sym pg, os, ln;
  lispm_pointer_unpack(p, &pg, &os, &ln);
  EVAL_CHECK(b <= ln, ERR_EVAL);
  int o;
  if (is_nil(l))
    l = unsigned_sub(ln, b, &o);
  else
    l = lispm_evquote(l);
  e = unsigned_add(b, l, &o);
  EVAL_CHECK(!e && e <= ln, ERR_EVAL);
  return lispm_alloc_pointer(pg, unsigned_add(os, b, &o), l);
}

static struct Builtin LRT0[] = {
    {"PROGRAM", PROGRAM}, {"GETC", GETC}, {"COPY", COPY}, {"ADD", ADD},
    {"MUL", MUL},         {"SUB", SUB},   {"SPAN", SPAN},
};