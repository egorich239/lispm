#include "lrt0.h"
#include "lispm.h"

#define M lispm

static inline unsigned lispm_page_access(Sym pg) {
  LISPM_ASSERT(lispm_sym_is_shortnum(pg));
  const unsigned page = lispm_shortnum_val(pg);
  void *b, *e;
  unsigned access;
  return page <= 1 ? 0 : (lispm_rt_page(page, &b, &e, &access), access);
}

static Sym lispm_literal_name_span(Sym s) {
  LISPM_ASSERT(lispm_sym_is_literal(s));
  unsigned offs = lispm_literal_str_offs(s);
  unsigned len = __builtin_strlen(lispm.strings + offs);
  Sym arr[3] = {lispm_make_shortnum(1), lispm_make_shortnum(offs), lispm_make_shortnum(len)};
  return lispm_st_obj_alloc(LISPM_ST_OBJ_SPAN, arr);
}

enum { PAGE_PROGRAM, PAGE_STRINGS };

void *lispm_page_loc(Sym pg, unsigned offs, unsigned elt_size) {
  LISPM_ASSERT(lispm_sym_is_shortnum(pg) && !(elt_size & (elt_size - 1)));
  unsigned page = lispm_shortnum_val(pg), access;
  void *ptr, *b, *e;
  switch (page) {
  case PAGE_PROGRAM:
    b = (void *)M.program, e = (void *)M.program_end;
    break;
  case PAGE_STRINGS:
    b = M.strings, e = M.strings_end;
    break;
  default:
    lispm_rt_page(page, &b, &e, &access);
  }
  ptr = b + offs * elt_size;
  LISPM_ASSERT(ptr < e);
  return ptr;
}

static Sym PROGRAM(Sym e) {
  LISPM_EVAL_CHECK(lispm_sym_is_nil(e), LISPM_ERR_EVAL, "no arguments expected, got: ", e);
  Sym arr[3] = {lispm_make_shortnum(0), lispm_make_shortnum(0), lispm_make_shortnum(lispm.program_end - lispm.program)};
  return lispm_st_obj_alloc(LISPM_ST_OBJ_SPAN, arr);
}
static Sym GETC(Sym a) {
  Sym ptr = lispm_evquote(a);
  LISPM_EVAL_CHECK(lispm_sym_is_span(ptr), LISPM_ERR_EVAL, "memory span expected, got: ", ptr);
  Sym *addr = lispm_st_obj_unpack(ptr);
  if (!lispm_shortnum_val(addr[2])) return lispm_shortnum_val(0); /* position right after the interval */
  return lispm_make_shortnum(*(unsigned char *)lispm_page_loc(addr[0], lispm_shortnum_val(addr[1]), 1));
}

static Sym COPY(Sym a) {
  Sym dest, src, dest_offs, src_offs, dest_len, src_len, *addr;
  lispm_args_unpack2(a, &dest, &src);
  LISPM_EVAL_CHECK(lispm_sym_is_span(dest) && lispm_sym_is_span(src), LISPM_ERR_EVAL,
                   "memory spans expected as args, got: ", a);
  addr = lispm_st_obj_unpack(dest), dest = addr[0], dest_offs = addr[1], dest_len = addr[2];
  addr = lispm_st_obj_unpack(src), src = addr[0], src_offs = addr[1], src_len = addr[2];
  LISPM_EVAL_CHECK(lispm_page_access(dest) & PAGE_ACCESS_W, LISPM_ERR_EVAL, "destination page must be writeable",
                   LISPM_SYM_NO_ASSOC);
  if (src_len < dest_len) dest_len = src_len;
  __builtin_memcpy(lispm_page_loc(dest, lispm_shortnum_val(dest_offs), 1),
                   lispm_page_loc(src, lispm_shortnum_val(src_offs), 1), lispm_shortnum_val(dest_len));
  return dest_len;
}
static Sym STR(Sym e) {
  e = lispm_evquote(e);
  LISPM_EVAL_CHECK(lispm_sym_is_literal(e), LISPM_ERR_EVAL, "literal expected, got: ", e);
  return lispm_literal_name_span(e);
}
static Sym CHARS(Sym a) {
  a = lispm_evquote(a);
  Sym pg, offs, len, *addr;
  LISPM_EVAL_CHECK(lispm_sym_is_span(a), LISPM_ERR_EVAL, "memory span expected, got: ", a);
  addr = lispm_st_obj_unpack(a), pg = addr[0], offs = addr[1], len = addr[2];
  const unsigned char *p = lispm_page_loc(pg, lispm_shortnum_val(offs), 1);
  unsigned l = lispm_shortnum_val(len);
  p += l;
  Sym res = LISPM_SYM_NIL;
  while (l--)
    res = lispm_cons_alloc(lispm_make_shortnum(*--p), res);
  return res;
}
static Sym SPAN(Sym args) {
  Sym span, start, len, *cons, *addr;
  cons = lispm_cons_unpack_user(args), span = cons[0], start = cons[1];
  cons = lispm_cons_unpack_user(start), start = cons[0], len = cons[1];
  LISPM_EVAL_CHECK(lispm_sym_is_span(span), LISPM_ERR_EVAL, "memory span expected, got: ", span);
  Sym page, offs, old_len;
  addr = lispm_st_obj_unpack(span), page = addr[0], offs = addr[1], old_len = addr[2];
  len = !lispm_sym_is_nil(len) ? lispm_evquote(len)
                               : lispm_make_shortnum(lispm_shortnum_val(old_len) - lispm_shortnum_val(start));
  /* TODO: proper check */
  // LISPM_EVAL_CHECK(len <= old_len, LISPM_ERR_EVAL, "subspan ");
  Sym arr[3] = {page, lispm_make_shortnum(lispm_shortnum_val(offs) + lispm_shortnum_val(start)), len};
  return lispm_st_obj_alloc(LISPM_ST_OBJ_SPAN, arr);
}
static Sym PARSE(Sym a) {
  a = lispm_evquote(a);
  LISPM_EVAL_CHECK(lispm_sym_is_span(a), LISPM_ERR_EVAL, "memory span expected, got:", a);
  Sym pg, offs, len, *addr;
  addr = lispm_st_obj_unpack(a), pg = addr[0], offs = addr[1], len = addr[2];
  LISPM_EVAL_CHECK(lispm_shortnum_val(pg) == PAGE_PROGRAM, LISPM_ERR_EVAL,
                   "parsing non-program page is prohibited, got: ", pg);
  const char *pc = lispm.program + lispm_shortnum_val(offs);
  return lispm_parse(pc, pc + lispm_shortnum_val(len));
}

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

LISPM_BUILTINS_EXT(LRT0) = {
    {"program", PROGRAM},
    {"span", SPAN},
    {"parse", PARSE},
    {"str", STR, lispm_evcap_quote},
    {"chars", CHARS},
    {"getc", GETC},
    {"copy", COPY},
    {"+", ADD},
    {"*", MUL},
    {"-", SUB},
    {"bitwise-and", BAND},
    {"bitwise-or", BOR},
    {"bitwise-xor", BXOR},
    {"bitwise-not", BNOT},
};