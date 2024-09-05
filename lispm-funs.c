#include "lispm-builtins.h"
#include "lispm-obj.h"
#include "lispm.h"

#define M lispm
#define C lispm_cons_alloc

LISPM_BUILTINS_EXT(LRT0_SYMS) = {{"#t"}, {"#modulo"}};
#define BUILTIN_T      (LRT0_SYMS + 0)
#define BUILTIN_MODULO (LRT0_SYMS + 1)

static Sym LIST(Sym a) { return a; }
static Sym CONS(Sym a) {
  Sym xy[2];
  LISPM_EVAL_CHECK(lispm_list_scan(xy, a, 2) == 2, a, panic, "two arguments expected, got: ", a);
  return C(xy[0], xy[1]);
}
static Sym CONS_UNPACK(Sym a, unsigned offs) {
  Sym arg;
  LISPM_EVAL_CHECK(lispm_list_scan(&arg, a, 1) == 1 && lispm_sym_is_cons(arg), a, panic,
                   "single cons-arguments expected, got: ", a);
  return lispm_st_obj_unpack(arg)[offs];
}
static Sym CAR(Sym a) { return CONS_UNPACK(a, 1); }
static Sym CDR(Sym a) { return CONS_UNPACK(a, 0); }
static Sym ATOM(Sym a) {
  Sym arg;
  LISPM_EVAL_CHECK(lispm_list_scan(&arg, a, 1) == 1, a, panic, "single arguments expected, got: ", a);
  return lispm_sym_is_atom(arg) ? lispm_sym_from_builtin(BUILTIN_T) : LISPM_SYM_NIL;
}
static Sym EQ(Sym a) {
  Sym xy[2];
  LISPM_EVAL_CHECK(lispm_list_scan(xy, a, 2) == 2, a, panic, "two arguments expected, got: ", a);
  return lispm_sym_is_atom(xy[0]) && xy[0] == xy[1] ? lispm_sym_from_builtin(BUILTIN_T) : LISPM_SYM_NIL;
}

typedef enum { OP_OR, OP_AND, OP_XOR, OP_ADD, OP_SUB, OP_MUL } BinOp;
static int binop_unpack(Sym args, int arith, Sym *p, Sym *q) {
  Sym pqm[3] = {};
  const unsigned argc = lispm_list_scan(pqm, args, arith ? 3 : 2);
  LISPM_EVAL_CHECK(((argc == 2) || (arith && argc == 3)) && lispm_sym_is_shortnum(pqm[0]) &&
                       lispm_sym_is_shortnum(pqm[1]) &&
                       (lispm_sym_is_nil(pqm[2]) || lispm_sym_from_builtin(BUILTIN_MODULO) == pqm[2]),
                   args, panic, "p, q[, #modulo] expected, got: ", args);
  *p = pqm[0], *q = pqm[1];
  return lispm_sym_from_builtin(BUILTIN_MODULO) == pqm[2];
}
static Sym binop(Sym args, BinOp op, int arith) {
  Sym p, q;
  int mod2 = binop_unpack(args, arith, &p, &q);
  LISPM_EVAL_CHECK(lispm_sym_is_shortnum(p) && lispm_sym_is_shortnum(q), args, panic,
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
  LISPM_EVAL_CHECK(mod2 || !overflow, args, panic, "integer overflow, args: ", args);
  return res;
}

static Sym BAND(Sym a) { return binop(a, OP_AND, 0); }
static Sym BOR(Sym a) { return binop(a, OP_OR, 0); }
static Sym BXOR(Sym a) { return binop(a, OP_XOR, 0); }
static Sym ADD(Sym a) { return binop(a, OP_ADD, 1); }
static Sym SUB(Sym a) { return binop(a, OP_SUB, 1); }
static Sym MUL(Sym a) { return binop(a, OP_MUL, 1); }

static Sym BNOT(Sym args) {
  Sym val;
  LISPM_EVAL_CHECK(lispm_list_scan(&val, args, 1) == 1 && lispm_sym_is_shortnum(val), args, panic,
                   "numeric value expected, got: ", args);
  return lispm_shortnum_bitwise_not(val);
}
static Sym NEG(Sym args) {
  Sym val;
  LISPM_EVAL_CHECK(lispm_list_scan(&val, args, 1) == 1 && lispm_sym_is_shortnum(val), args, panic,
                   "numeric value expected, got: ", args);
  return lispm_shortnum_neg(val);
}

LISPM_BUILTINS_EXT(LISPM_FUNS) = {
    {"atom?",       ATOM},
    {"eq?",         EQ  },
    {"list",        LIST},
    {"cons",        CONS},
    {"car",         CAR },
    {"cdr",         CDR },
    {"+",           ADD },
    {"*",           MUL },
    {"-",           SUB },
    {"~",           NEG },
    {"bitwise-and", BAND},
    {"bitwise-or",  BOR },
    {"bitwise-xor", BXOR},
    {"bitwise-not", BNOT},
};
