#include <liblispm/builtins.h>
#include <liblispm/lispm.h>
#include <liblispm/obj.h>

/**/
#include <liblispm/internal-macros.h>

LISPM_BUILTINS_EXT(LRT0_SYMS) = {
    {"#t", 0, 0, LISPM_BUILTIN_LITERAL_SELFREF},
    {":modulo"}
};
#define BUILTIN_T      (LRT0_SYMS + 0)
#define BUILTIN_MODULO (LRT0_SYMS + 1)

static Obj LIST(Obj a) { return lispm_return0(a); }
static Obj CONS(Obj a) {
  Obj xy[2];
  LISPM_EVAL_CHECK(lispm_list_scan(xy, a, 2) == 2, a, panic, "two arguments expected, got: ", a);
  return lispm_return0(C(xy[0], xy[1]));
}
static Obj CONS_UNPACK(Obj a, unsigned offs) {
  Obj arg;
  LISPM_EVAL_CHECK(lispm_list_scan(&arg, a, 1) == 1 && lispm_obj_is_cons(arg), a, panic,
                   "single cons-arguments expected, got: ", a);
  return lispm_return0(lispm_obj_unpack(arg)[offs]);
}
static Obj CAR(Obj a) { return CONS_UNPACK(a, 1); }
static Obj CDR(Obj a) { return CONS_UNPACK(a, 0); }
static Obj ATOM(Obj a) {
  Obj arg;
  LISPM_EVAL_CHECK(lispm_list_scan(&arg, a, 1) == 1, a, panic, "single arguments expected, got: ", a);
  return lispm_return0(lispm_obj_is_atom(arg) ? lispm_obj_from_builtin(BUILTIN_T) : NIL);
}
static Obj EQ(Obj a) {
  Obj xy[2];
  LISPM_EVAL_CHECK(lispm_list_scan(xy, a, 2) == 2, a, panic, "two arguments expected, got: ", a);
  return lispm_return0(lispm_obj_is_atom(xy[0]) && xy[0] == xy[1] ? lispm_obj_from_builtin(BUILTIN_T) : NIL);
}

typedef enum { OP_OR, OP_AND, OP_XOR, OP_ADD, OP_SUB, OP_MUL } BinOp;
static int binop_unpack(Obj args, int arith, Obj *p, Obj *q) {
  Obj pqm[3] = {};
  const unsigned argc = lispm_list_scan(pqm, args, arith ? 3 : 2);
  LISPM_EVAL_CHECK(((argc == 2) || (arith && argc == 3)) && lispm_obj_is_shortnum(pqm[0]) &&
                       lispm_obj_is_shortnum(pqm[1]) &&
                       (lispm_obj_is_nil(pqm[2]) || lispm_obj_from_builtin(BUILTIN_MODULO) == pqm[2]),
                   args, panic, "p, q[, :modulo] expected, got: ", args);
  *p = pqm[0], *q = pqm[1];
  return lispm_obj_from_builtin(BUILTIN_MODULO) == pqm[2];
}
static Obj binop(Obj args, BinOp op, int arith) {
  Obj p, q;
  int mod2 = binop_unpack(args, arith, &p, &q);
  LISPM_EVAL_CHECK(lispm_obj_is_shortnum(p) && lispm_obj_is_shortnum(q), args, panic,
                   "both operands must be numeric, got: ", args);
  int overflow = 0;
  Obj res;
  switch (op) {
  case OP_OR:
    res = lispm_shortnum_bitwise_or(p, q);
    break;
  case OP_AND:
    res = lispm_shortnum_bitwise_and(p, q);
    break;
  case OP_XOR:
    res = lispm_shortnum_bitwise_xor(p, q);
    break;
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
  return lispm_return0(res);
}

static Obj BAND(Obj a) { return binop(a, OP_AND, 0); }
static Obj BOR(Obj a) { return binop(a, OP_OR, 0); }
static Obj BXOR(Obj a) { return binop(a, OP_XOR, 0); }
static Obj ADD(Obj a) { return binop(a, OP_ADD, 1); }
static Obj SUB(Obj a) { return binop(a, OP_SUB, 1); }
static Obj MUL(Obj a) { return binop(a, OP_MUL, 1); }

static Obj BNOT(Obj args) {
  Obj val;
  LISPM_EVAL_CHECK(lispm_list_scan(&val, args, 1) == 1 && lispm_obj_is_shortnum(val), args, panic,
                   "numeric value expected, got: ", args);
  return lispm_return0(lispm_shortnum_bitwise_not(val));
}
static Obj NEG(Obj args) {
  Obj val;
  LISPM_EVAL_CHECK(lispm_list_scan(&val, args, 1) == 1 && lispm_obj_is_shortnum(val), args, panic,
                   "numeric value expected, got: ", args);
  return lispm_return0(lispm_shortnum_neg(val));
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
