#pragma once

#define TABLE_PREFIX                                                           \
  "NIL\0"                                                                      \
  "QUOTE\0\0\0"                                                                \
  "COND\0\0\0\0"                                                               \
  "LET\0"                                                                      \
  "LAMBDA\0\0"                                                                 \
  "T\0\0\0"                                                                    \
  "EQ\0\0"                                                                     \
  "ATOM\0\0\0\0"                                                               \
  "CAR\0"                                                                      \
  "CDR\0"                                                                      \
  "CONS\0\0\0\0"                                                               \
  "STR\0"                                                                      \
  "!LEX\0\0\0\0"                                                               \
  "!PARSE\0\0"                                                                 \
  "!EVAL\0\0\0"                                                                \
  "!OOM\0\0\0\0"                                                               \
  "!INVALID_SYM\0\0\0\0"

#define HTABLE_OFFSET 512

#define SYM_NIL 0u
#define SYM_QUOTE 4u
#define SYM_COND 12u
#define SYM_LET 20u
#define SYM_LAMBDA 24u
#define SYM_T 32u
#define SYM_EQ 36u
#define SYM_ATOM 40u
#define SYM_CAR 48u
#define SYM_CDR 52u
#define SYM_CONS 56u
#define SYM_STR 64u
#define ERR_LEX 68u
#define ERR_PARSE 76u
#define ERR_EVAL 84u
#define ERR_OOM 92u
#define ERR_INVALID_SYM 100u
