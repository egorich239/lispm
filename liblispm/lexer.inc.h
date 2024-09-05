/*
lexer char categories:
- 00: literal symbol;
- 01: digit;
- 10: token symbol (currently only parens and quote);
- 11: special symbols; those are either lex'd specially (e.g. semicolon), or are
errors.

We utilize the following properties:
1. if the higher bit is 0, then it is a valid atom symbol;
2. if the higher bit is 1, then it is a delimiter symbol.
*/
__attribute__((aligned(8))) static const unsigned char LEX_CHARS[32] = {
    0b11111111, /* 0x00 */
    0b11111111, /* 0x04*/
    0b11111111, /* 0x08 */
    0b11111111, /* 0x0C */
    0b11111111, /* 0x10 */
    0b11111111, /* 0x14 */
    0b11111111, /* 0x18 */
    0b11111111, /* 0x1C */
    0b11100011, /* #"!<SP> */
    0b10000000, /* '&%$ */
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

#define LEX_CHAR_CAT(x)      ((LEX_CHARS[(x) >> 2] >> (((x) & 3) << 1)) & 3)
#define LEX_IS_DELIM(cat)    (((cat) & 2) == 2)
#define LEX_IS_ATOM_SYM(cat) (((cat) & 2) == 0)
#define LEX_IS_DIGIT(cat)    (((cat) & 3) == 1)
#define LEX_IS_TOK(cat)      (((cat) & 3) == 2)