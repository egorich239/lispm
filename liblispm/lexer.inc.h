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
    0b00000000, /* 0x00 */
    0b00000000, /* 0x04*/
    0b00000000, /* 0x08 */
    0b00000000, /* 0x0C */
    0b00000000, /* 0x10 */
    0b00000000, /* 0x14 */
    0b00000000, /* 0x18 */
    0b00000000, /* 0x1C */
    0b01011000, /* #"!<SP> */
    0b00101010, /* '&%$ */
    0b10100000, /* +*)( */
    0b10101001, /* /.-, */
    0b11111111, /* 3210 */
    0b11111111, /* 7654 */
    0b01101111, /* ;:98 */
    0b10101010, /* ?>=< */
    0b10101010, /* CBA@ */
    0b10101010, /* GFED */
    0b10101010, /* KJIH */
    0b10101010, /* ONML */
    0b10101010, /* SPQR :-) */
    0b10101010, /* WVUT */
    0b01101010, /* [ZYX */
    0b10100101, /* _^]\ */
    0b10101001, /* cba` */
    0b10101010, /* gfed */
    0b10101010, /* kjih */
    0b10101010, /* onml */
    0b10101010, /* srqp */
    0b10101010, /* wvut */
    0b01101010, /* {zyx */
    0b01100101, /* <DEL>~}| */
};

#define LEX_CHAR_CAT(x)      ((LEX_CHARS[(x) >> 2] >> (((x) & 3) << 1)) & 3)
#define LEX_IS_ATOM_SYM(cat) ((cat) & 2)
#define LEX_IS_DELIM(cat)    ((cat) == 0)
#define LEX_IS_SPECIAL(cat)  ((cat) == 1)
#define LEX_IS_DIGIT(cat)    ((cat) == 3)