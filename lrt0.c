#include "lispm.h"
#include "sym.h"

static unsigned num_decode(Sym s) {
  EVAL_CHECK(lispm_sym_is_shortnum(s) || lispm_sym_is_longnum(s), LISPM_ERR_EVAL);
  if (lispm_sym_is_shortnum(s)) return lispm_shortnum_val(s);

  Sym hi, lo;
  lispm_st_obj_unpack2(s, &hi, &lo);
  EVAL_CHECK(lispm_sym_is_shortnum(lo), LISPM_ERR_EVAL);
  return (lispm_shortnum_val(hi) << LISPM_SHORTNUM_BITS) | lispm_shortnum_val(lo);
}
static Sym num_encode(unsigned e) {
  if (lispm_shortnum_can_represent(e)) return lispm_make_shortnum(e);
  Sym arr[2] = {lispm_make_shortnum(e >> LISPM_SHORTNUM_BITS),
                lispm_make_shortnum(e & ((1u << LISPM_SHORTNUM_BITS) - 1))};
  return lispm_st_obj_alloc(LISPM_ST_OBJ_LONGNUM, arr);
}

static Sym CONS(Sym a) {
  Sym x, y;
  lispm_args_unpack2(a, &x, &y);
  return lispm_cons_alloc(x, y);
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
static Sym ATOM(Sym a) { return lispm_sym_is_atom(lispm_evquote(a)) ? LISPM_SYM_T : LISPM_SYM_NIL; }
static Sym EQ(Sym a) {
  Sym x, y;
  lispm_args_unpack2(a, &x, &y);
  if (!lispm_sym_is_atom(x) || !lispm_sym_is_atom(y)) return LISPM_SYM_NIL;
  if (x == y) return LISPM_SYM_T;
  if (!lispm_sym_is_longnum(x) || !lispm_sym_is_longnum(y)) return LISPM_SYM_NIL;
  return num_decode(x) == num_decode(y) ? LISPM_SYM_T : LISPM_SYM_NIL;
}

static Sym PROGRAM(Sym e) {
  EVAL_CHECK(lispm_sym_is_nil(e), LISPM_ERR_EVAL);
  Sym arr[3] = {LISPM_PAGE_PROGRAM, lispm_make_shortnum(0),
                lispm_make_shortnum(lispm_page_size(LISPM_PAGE_PROGRAM, 0))};
  return lispm_st_obj_alloc(LISPM_ST_OBJ_SPAN, arr);
}
static Sym GETC(Sym a) {
  Sym page, offs, len, ptr = lispm_evquote(a);
  EVAL_CHECK(lispm_sym_is_span(ptr), LISPM_ERR_EVAL);
  lispm_st_obj_unpack3(ptr, &page, &offs, &len);
  if (!lispm_shortnum_val(len)) return lispm_shortnum_val(0); /* position right after the interval */
  return lispm_make_shortnum(*(unsigned char *)lispm_page_loc(page, lispm_shortnum_val(offs), 1));
}
static Sym COPY(Sym a) {
  Sym dest, src, dest_offs, src_offs, dest_len, src_len;
  lispm_args_unpack2(a, &dest, &src);
  EVAL_CHECK(lispm_sym_is_span(dest) && lispm_sym_is_span(src), LISPM_ERR_EVAL);
  lispm_st_obj_unpack3(dest, &dest, &dest_offs, &dest_len);
  lispm_st_obj_unpack3(src, &src, &src_offs, &src_len);
  struct PageDesc *dest_page = lispm_page_desc(dest);
  EVAL_CHECK(dest_page->flags == PAGE_FLAG_RW, LISPM_ERR_EVAL);
  if (src_len < dest_len) dest_len = src_len;
  __builtin_memcpy(lispm_page_loc(dest, lispm_shortnum_val(dest_offs), 1),
                   lispm_page_loc(src, lispm_shortnum_val(src_offs), 1), lispm_shortnum_val(dest_len));
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
  lispm_st_obj_unpack3(a, &pg, &offs, &len);
  const unsigned char *p = lispm_page_loc(pg, lispm_shortnum_val(offs), 1);
  unsigned l = lispm_shortnum_val(len);
  p += l;
  Sym res = LISPM_SYM_NIL;
  while (l--)
    res = lispm_cons_alloc(lispm_make_shortnum(*--p), res);
  return res;
}

static Sym BAND(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  return num_encode(num_decode(p) & num_decode(q));
}
static Sym BNOT(Sym a) {
  a = lispm_evquote(a);
  return num_encode(~num_decode(a));
}
static Sym ADD(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  EVAL_CHECK(lispm_sym_is_shortnum(p) && lispm_sym_is_shortnum(q), LISPM_ERR_EVAL);
  unsigned res;
  int overflow = __builtin_uadd_overflow(num_decode(p), num_decode(q), &res);
  EVAL_CHECK(!overflow, LISPM_ERR_EVAL);
  return num_encode(res);
}
static Sym MUL(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  EVAL_CHECK(lispm_sym_is_shortnum(p) && lispm_sym_is_shortnum(q), LISPM_ERR_EVAL);
  unsigned res;
  int overflow = __builtin_umul_overflow(num_decode(p), num_decode(q), &res);
  EVAL_CHECK(!overflow, LISPM_ERR_EVAL);
  return num_encode(res);
}
static Sym SUB(Sym a) {
  Sym p, q;
  lispm_args_unpack2(a, &p, &q);
  EVAL_CHECK(lispm_sym_is_shortnum(p) && lispm_sym_is_shortnum(q), LISPM_ERR_EVAL);
  unsigned res;
  int overflow = __builtin_usub_overflow(num_decode(p), num_decode(q), &res);
  EVAL_CHECK(!overflow, LISPM_ERR_EVAL);
  return num_encode(res);
}
static Sym SPAN(Sym args) {
  Sym span, start, len;
  lispm_cons_unpack_user(args, &span, &start);
  lispm_cons_unpack_user(start, &start, &len);
  EVAL_CHECK(lispm_sym_is_span(span), LISPM_ERR_EVAL);
  Sym page, offs, old_len;
  lispm_st_obj_unpack3(span, &page, &offs, &old_len);
  EVAL_CHECK(start <= old_len, LISPM_ERR_EVAL);
  len = !lispm_sym_is_nil(len) ? lispm_evquote(len)
                               : lispm_make_shortnum(lispm_shortnum_val(old_len) - lispm_shortnum_val(start));
  EVAL_CHECK(len <= old_len, LISPM_ERR_EVAL);
  Sym arr[3] = {page, lispm_make_shortnum(lispm_shortnum_val(offs) + lispm_shortnum_val(start)), len};
  return lispm_st_obj_alloc(LISPM_ST_OBJ_SPAN, arr);
}
static Sym PARSE(Sym a) {
  a = lispm_evquote(a);
  EVAL_CHECK(lispm_sym_is_span(a), LISPM_ERR_EVAL);
  Sym pg, offs, len;
  lispm_st_obj_unpack3(a, &pg, &offs, &len);
  return lispm_parse(lispm_page_loc(pg, lispm_shortnum_val(offs), 1));
}

struct Builtin LRT0[] = {
    {"CONS", CONS},
    {"CAR", CAR},
    {"CDR", CDR},
    {"ATOM", ATOM},
    {"EQ", EQ},
    {"PROGRAM", PROGRAM},
    {"STR", STR, lispm_evcap_quote},
    {"CHARS", CHARS},
    {"GETC", GETC},
    {"COPY", COPY},
    {"SPAN", SPAN},
    {"ADD", ADD},
    {"MUL", MUL},
    {"SUB", SUB},
    {"BAND", BAND},
    {"BNOT", BNOT},
    {"PARSE", PARSE},
    {},
};