/*
lexer char categories:
- 00: digit;
- 01: literal symbol;
- 10: token symbol (currently only parens and quote);
- 11: special symbols; those are either lex'd specially (e.g. semicolon), or are
errors.

We utilize the following properties:
1. if the higher bit is 0, then it is a valid atom symbol;
2. if the higher bit is 1, then it is a delimiter symbol.
*/
__attribute__((aligned(8))) static const unsigned char LEX_CHARS[32] = {
    0b10101010, /* 0x00 */
    0b10101010, /* 0x04*/
    0b10101010, /* 0x08 */
    0b10101010, /* 0x0C */
    0b10101010, /* 0x10 */
    0b10101010, /* 0x14 */
    0b10101010, /* 0x18 */
    0b10101010, /* 0x1C */
    0b11100110, /* #"!<SP> */
    0b10010101, /* '&%$ */
    0b01011010, /* +*)( */
    0b01010111, /* /.-, */
    0b00000000, /* 3210 */
    0b00000000, /* 7654 */
    0b10010000, /* ;:98 */
    0b01010101, /* ?>=< */
    0b01010101, /* CBA@ */
    0b01010101, /* GFED */
    0b01010101, /* KJIH */
    0b01010101, /* ONML */
    0b01010101, /* SPQR :-) */
    0b01010101, /* WVUT */
    0b11010101, /* [ZYX */
    0b01011111, /* _^]\ */
    0b01010111, /* cba` */
    0b01010101, /* gfed */
    0b01010101, /* kjih */
    0b01010101, /* onml */
    0b01010101, /* srqp */
    0b01010101, /* wvut */
    0b11010101, /* {zyx */
    0b11011111, /* <DEL>~}| */
};

#define LEX_CHAR_CAT(x)      ((LEX_CHARS[(x) >> 2] >> (((x) & 3) << 1)) & 3)
#define LEX_IS_ATOM_SYM(cat) (((cat) & 2) == 0)
#define LEX_IS_DIGIT(cat)    ((cat) == 0)
#define LEX_IS_DELIM(cat)    ((cat) == 2)
#define LEX_IS_SPECIAL(cat)  ((cat) == 3)