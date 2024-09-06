#pragma once
/**
 * This file contains macros common for .c implementations of lispm.
 * These macros are named short to very short, so it is not wise to
 * include them from a public header file.
 */

#define Obj LispmObj

#define M lispm
#define C lispm_cons_alloc
#define C_UNPACK(c, car, cdr)                                                                                          \
  {                                                                                                                    \
    LispmObj li_ = (c);                                                                                                \
    LISPM_ASSERT(lispm_obj_is_cons(li_));                                                                              \
    LispmObj *cons_ = lispm_obj_unpack(li_);                                                                           \
    car = cons_[1], cdr = cons_[0];                                                                                    \
  }
#define C_CAR(c) lispm_obj_unpack(c)[1]
#define C_CDR(c) lispm_obj_unpack(c)[0]
#define C_ENSURE(seq, msg)                                                                                             \
  {                                                                                                                    \
    LispmObj v_ = (seq);                                                                                               \
    LISPM_EVAL_CHECK(lispm_obj_is_nil(v_) || lispm_obj_is_cons(v_), v_, panic, msg ", got: ", v_);                     \
  }

#define T lispm_triplet_alloc
#define T_UNPACK(t, a, b, n)                                                                                           \
  {                                                                                                                    \
    LispmObj li_ = (t);                                                                                                \
    LISPM_ASSERT(lispm_obj_is_triplet(li_));                                                                           \
    LispmObj *cons_ = lispm_obj_unpack(li_);                                                                           \
    n = cons_[0], a = cons_[1], b = cons_[2];                                                                          \
  }
#define T_1(t)    lispm_obj_unpack(t)[1]
#define T_2(t)    lispm_obj_unpack(t)[2]
#define T_NEXT(t) lispm_obj_unpack(t)[0]

#define FOR_EACH_C(arg, seq)                                                                                           \
  for (LispmObj * cons_, it_ = (seq), arg;                                                                             \
       !lispm_obj_is_nil(it_) && (cons_ = lispm_obj_unpack(it_), it_ = cons_[0], arg = cons_[1], 1);)

#define FOR_EACH_T(a, b, seq)                                                                                          \
  for (LispmObj * cons_, it_ = (seq), a, b;                                                                            \
       !lispm_obj_is_nil(it_) && (cons_ = lispm_obj_unpack(it_), it_ = cons_[0], a = cons_[1], b = cons_[2], 1);)

#define TRACE_NATIVE_STACK()                                                                                           \
  do {                                                                                                                 \
    LISPM_TRACE(stack_depth, LISPM_TRACE_STACK_NATIVE, lispm_rt_stack_depth(M.stack_bottom_mark));                     \
    LISPM_EVAL_CHECK(lispm_rt_stack_depth(M.stack_bottom_mark) < M.stack_depth_limit, LISPM_SYM_NIL, oom_stack);       \
  } while (0)
