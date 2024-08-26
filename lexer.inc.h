/*
lexer char categories:
- 00: literal symbol;
- 01: digit;
- 10: token symbol (currently only parens and quote);
- 11: special symbols; those are either lex'd specially (e.g. semicolon), or are errors.

We utilize the following properties:
1. if the higher bit is 0, then it is a valid atom symbol;
2. if the higher bit is 1, then it is a delimiter symbol.
*/
__attribute__((aligned(8))) static const unsigned char LEX_CHARS[24] = {
    0b11100011, /* #"!<SP> */
    0b11000000, /* '&%$ */
    0b00001010, /* +*)( */
    0b00000011, /* /.-, */
    0b01010101, /* 3210 */
    0b01010101, /* 7654 */
    0b11000101, /* ;:98 */
    0b00000000, /* ?>=< */
    0b00000000, /* CBA@ */
    0b00000000, /* GFED */
    0b00000000, /* KJIH */
    0b00000000, /* ONML */
    0b00000000, /* SPQR :-) */
    0b00000000, /* WVUT */
    0b11000000, /* [ZYX */
    0b00001111, /* _^]\ */
    0b00000011, /* cba` */
    0b00000000, /* gfed */
    0b00000000, /* kjih */
    0b00000000, /* onml */
    0b00000000, /* srqp */
    0b00000000, /* wvut */
    0b11000000, /* {zyx */
    0b11001111, /* <DEL>~}| */
};

#define LEX_CHAR_CAT(x)    ((LEX_CHARS[((x) - 32) >> 2] >> (((x) - 32) & 3)) & 3)
#define LEX_IS_DELIM(x)    ((LEX_CHAR_CAT(x) & 2) == 1)
#define LEX_IS_ATOM_SYM(x) ((LEX_CHAR_CAT(x) & 2) == 0)
#define LEX_IS_DIGIT(x)    ((LEX_CHAR_CAT(x) & 3) == 1)
#define LEX_IS_TOK(x)      ((LEX_CHAR_CAT(x) & 3) == 2)