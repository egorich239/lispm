#include "lrt0.h"
#include "lispm-builtins.h"
#include "lispm.h"

#define M lispm

enum { PAGE_PROGRAM = 0, PAGE_STRINGS = 1 };

static void check_access_read(unsigned access, Sym ptr) {
  LISPM_EVAL_CHECK(access & PAGE_ACCESS_R, ptr, panic, "read access denied: ", ptr);
}
static void check_access_write(unsigned access, Sym ptr) {
  LISPM_EVAL_CHECK(access & PAGE_ACCESS_W, ptr, panic, "write access denied: ", ptr);
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
  LISPM_EVAL_CHECK(lispm_sym_is_span(span), span, panic, "span expected, got: ", span);
  return lispm_span_unpack(span);
}

static Sym PROGRAM(Sym e) {
  LISPM_EVAL_CHECK(lispm_sym_is_nil(e), e, panic, "no arguments expected, got: ", e);
  return lispm_span_alloc(lispm_make_span(PAGE_PROGRAM, 0, M.program_end - M.program));
}
static Sym GETC(Sym args) {
  Sym ptr;
  LISPM_EVAL_CHECK(lispm_list_scan(&ptr, args, 1) == 1 && lispm_sym_is_span(ptr), args, panic,
                   "one span argument expected, got: ", args);
  unsigned len, access;
  struct Span span = lispm_span_unpack(ptr);
  const unsigned char *c = lispm_span_base(span, &len, &access);
  check_access_read(access, ptr);
  if (!len) return LISPM_SYM_NIL; /* position right after the interval */
  return lispm_make_shortnum(*c);
}
static Sym COPY(Sym args) {
  Sym ds[2];
  LISPM_EVAL_CHECK(lispm_list_scan(ds, args, 2) == 2 && lispm_sym_is_span(ds[0]) && lispm_sym_is_span(ds[1]), args,
                   panic, "two span argument expected, got: ", args);
  struct Span dest = lispm_span_unpack(ds[0]), src = lispm_span_unpack(ds[1]);
  unsigned dest_len, dest_access, src_len, src_access;
  void *dest_ptr = lispm_span_base(dest, &dest_len, &dest_access),
       *src_ptr = lispm_span_base(src, &src_len, &src_access);
  LISPM_EVAL_CHECK(src_len <= dest_len, args, panic, "copy out of bounds for args: ", args);
  check_access_read(src_access, ds[0]);
  check_access_write(dest_access, ds[1]);
  __builtin_memcpy(dest_ptr, src_ptr, src_len);
  dest.len = lispm_make_shortnum(src_len);
  return lispm_span_alloc(dest);
}
static Sym STR(Sym args) {
  Sym lit;
  LISPM_EVAL_CHECK(lispm_list_scan(&lit, args, 1) == 1 && lispm_sym_is_literal(lit), args, panic,
                   "one literal argument expected, got: ", args);
  return lispm_literal_name_span(lit);
}
static Sym CHARS(Sym args) {
  Sym ptr;
  LISPM_EVAL_CHECK(lispm_list_scan(&ptr, args, 1) == 1 && lispm_sym_is_span(ptr), args, panic,
                   "one span argument expected, got: ", args);
  unsigned len, access;
  const unsigned char *base = lispm_span_base(lispm_span_unpack(ptr), &len, &access);
  check_access_read(access, ptr);
  Sym res = LISPM_SYM_NIL;
  for (const unsigned char *p = base + len; p != base;)
    res = lispm_cons_alloc(lispm_make_shortnum(*--p), res);
  return res;
}
static Sym SPAN(Sym args) {
  Sym sbe[3] = {}; /* span, begin, end */
  unsigned argc = lispm_list_scan(sbe, args, 3);
  LISPM_EVAL_CHECK((argc == 2 || argc == 3) && lispm_sym_is_span(sbe[0]) && lispm_sym_is_shortnum(sbe[1]) &&
                       (lispm_sym_is_nil(sbe[2]) || lispm_sym_is_shortnum(sbe[2])),
                   args, panic, "span, begin[, end] arguments expected, got: ", args);
  struct Span res = lispm_span_unpack_user(sbe[0]);
  unsigned src_offs = lispm_shortnum_val(res.offs);
  unsigned src_len = lispm_shortnum_val(res.len);

  unsigned res_begin = lispm_shortnum_val(sbe[1]);
  unsigned res_end = !lispm_sym_is_nil(sbe[2]) ? lispm_shortnum_sval(sbe[2]) : src_len;
  if (((int)res_end) < 0) res_end = src_len + res_end;
  LISPM_EVAL_CHECK(res_begin <= res_end && res_end <= src_len, sbe[0], panic, "subspan does not fit: ", sbe[0]);

  /* we rely on the fact that all LISP-visible span constructors allocate shortnum-pages,
     and all operations can only shrink their input spans,
     so here we do not eval-check that the resulting values fit into shortnum,
     although we do indirectly assert-check.
  */
  res.offs = lispm_make_shortnum(src_offs + res_begin);
  res.len = lispm_make_shortnum(res_end - res_begin);
  return lispm_span_alloc(res);
}
static void lispm_program_get(Sym args, const char **begin, const char **end) {
  Sym ptr;
  LISPM_EVAL_CHECK(lispm_list_scan(&ptr, args, 1) == 1 && lispm_sym_is_span(ptr), args, panic,
                   "one span argument expected, got: ", args);
  const struct Span span = lispm_span_unpack(ptr);
  LISPM_EVAL_CHECK(lispm_shortnum_val(span.page) == PAGE_PROGRAM, ptr, panic,
                   "parsing non-program page is prohibited, got: ", ptr);
  unsigned access, len;
  *begin = lispm_span_base(span, &len, &access);
  *end = *begin + len;
}
static Sym PARSE(Sym args) {
  const char *begin, *end;
  lispm_program_get(args, &begin, &end);
  return lispm_parse_quote(begin, end);
}
static Sym IMPORT(Sym ptr) {
  /* the important difference from eval, is that parsing happens in-place,
     and the result of the parsing is evaluated immediately,
     which avoids late binding issues */
  const char *begin, *end;
  lispm_program_get(ptr, &begin, &end);
  return lispm_eval(begin, end);
}

LISPM_BUILTINS_EXT(LRT0) = {
    {"program",     PROGRAM},
    {"import",      IMPORT },
    {"parse",       PARSE  },
    {"span",        SPAN   },
    {"str",         STR    },
    {"chars",       CHARS  },
    {"getc",        GETC   },
    {"copy",        COPY   },
};