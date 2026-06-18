\ regex.f - bounded capture-free regex scanner/tokenizer.

$2E constant RX-C-DOT
$5E constant RX-C-CARET
$24 constant RX-C-DOLLAR
$5B constant RX-C-LBRACKET
$5D constant RX-C-RBRACKET
$5C constant RX-C-BACKSLASH
$3F constant RX-C-QUESTION
$2A constant RX-C-STAR
$2B constant RX-C-PLUS
$2D constant RX-C-DASH
$28 constant RX-C-LPAREN
$29 constant RX-C-RPAREN
$7B constant RX-C-LBRACE
$7D constant RX-C-RBRACE
$7C constant RX-C-BAR
255 constant RX-TOK-MAX-LEN

1 constant RX-TOK-LITERAL
2 constant RX-TOK-DOT
3 constant RX-TOK-BOL
4 constant RX-TOK-EOL
5 constant RX-TOK-CLASS
6 constant RX-TOK-NCLASS
7 constant RX-TOK-QUESTION
8 constant RX-TOK-STAR
9 constant RX-TOK-PLUS

: RX-ESCAPABLE? ( n -- bool )
   dup RX-C-DOT =
   over RX-C-CARET = or
   over RX-C-DOLLAR = or
   over RX-C-LBRACKET = or
   over RX-C-RBRACKET = or
   over RX-C-BACKSLASH = or
   over RX-C-QUESTION = or
   over RX-C-STAR = or
   over RX-C-PLUS = or
   over RX-C-DASH = or
   over RX-C-LPAREN = or
   over RX-C-RPAREN = or
   over RX-C-LBRACE = or
   over RX-C-RBRACE = or
   swap RX-C-BAR = or ;

: RX-UNSUPPORTED-META? ( n -- bool )
   dup RX-C-RBRACKET =
   over RX-C-LPAREN = or
   over RX-C-RPAREN = or
   over RX-C-LBRACE = or
   over RX-C-RBRACE = or
   swap RX-C-BAR = or ;

: RX-CHECK-BYTE ( n -- ) {: c :}
   c 0 < if E-RX-SYNTAX throw then
   c RX-TOK-MAX-LEN > if E-RX-SYNTAX throw then ;

: RX-NEED ( n n n -- ) {: cap out add :}
   cap 0 < if E-RX-CAPACITY throw then
   out 0 < if E-RX-CAPACITY throw then
   add 0 < if E-RX-CAPACITY throw then
   add cap out - > if E-RX-CAPACITY throw then ;

: RX-EMIT-1 ( n ptr u8 n n -- n ) {: op dst:ptr cap out :}
   op RX-CHECK-BYTE
   cap out 1 RX-NEED
   op dst out + c!
   out 1 + ;

: RX-EMIT-LIT ( n ptr u8 n n -- n ) {: c dst:ptr cap out :}
   c RX-CHECK-BYTE
   cap out 2 RX-NEED
   RX-TOK-LITERAL dst out + c!
   c dst out 1 + + c!
   out 2 + ;

: RX-EMIT-RANGE ( n ptr u8 n ptr u8 n n -- n ) {: op src:ptr raw dst:ptr cap out :}
   raw 0 <= if E-RX-SYNTAX throw then
   raw RX-TOK-MAX-LEN > if E-RX-CAPACITY throw then
   op RX-CHECK-BYTE
   cap out raw 2 + RX-NEED
   op dst out + c!
   raw dst out 1 + + c!
   src dst out 2 + + raw BYTE-COPY
   out raw + 2 + ;

: RX-SCAN-CLASS-BODY ( ptr u8 n n -- n ) {: a:ptr u idx0 :}
   idx0 begin dup u < while
      dup a + c@
      dup RX-C-RBRACKET = if
         drop dup idx0 = if E-RX-SYNTAX throw then
         exit
      then
      RX-C-BACKSLASH = if
         dup 1 + u >= if E-RX-SYNTAX throw then
         a over 1 + + c@ RX-ESCAPABLE? 0= if E-RX-SYNTAX throw then
         2 +
      else
         1 +
      then
   repeat drop E-RX-SYNTAX throw 0 ;

: RX-SCAN-CLASS ( ptr u8 n n -- n n n ) {: a:ptr u start :}
   start 1 + u >= if E-RX-SYNTAX throw then
   a start 1 + + c@ RX-C-CARET = if
      start 2 + dup u >= if E-RX-SYNTAX throw then
      dup a u rot RX-SCAN-CLASS-BODY
      1 exit
   then
   start 1 + dup a u rot RX-SCAN-CLASS-BODY
   0 ;

: RX-EMIT-CLASS-DONE ( ptr u8 ptr u8 n n n n n -- n n ) {: a:ptr dst:ptr cap out body close neg :}
   neg 0= if RX-TOK-CLASS else RX-TOK-NCLASS then
   a body + close body - dst cap out RX-EMIT-RANGE
   close 1 + ;

: RX-EMIT-CLASS ( ptr u8 n n ptr u8 n n -- n n ) {: a:ptr u start dst:ptr cap out :}
   a dst cap out a u start RX-SCAN-CLASS RX-EMIT-CLASS-DONE ;

: RX-SCAN-ESCAPE ( ptr u8 n n ptr u8 n n -- n n ) {: a:ptr u idx dst:ptr cap out :}
   idx 1 + u >= if E-RX-SYNTAX throw then
   a idx 1 + + c@ dup RX-ESCAPABLE? 0= if E-RX-SYNTAX throw then
   dst cap out RX-EMIT-LIT
   idx 2 + swap ;

: RX-SCAN-ONE ( ptr u8 n n ptr u8 n n -- n n ) {: a:ptr u idx dst:ptr cap out :}
   a idx + c@
   dup RX-C-BACKSLASH = if drop a u idx dst cap out RX-SCAN-ESCAPE exit then
   dup RX-C-DOT = if drop RX-TOK-DOT dst cap out RX-EMIT-1 idx 1 + swap exit then
   dup RX-C-CARET = if drop RX-TOK-BOL dst cap out RX-EMIT-1 idx 1 + swap exit then
   dup RX-C-DOLLAR = if drop RX-TOK-EOL dst cap out RX-EMIT-1 idx 1 + swap exit then
   dup RX-C-QUESTION = if drop RX-TOK-QUESTION dst cap out RX-EMIT-1 idx 1 + swap exit then
   dup RX-C-STAR = if drop RX-TOK-STAR dst cap out RX-EMIT-1 idx 1 + swap exit then
   dup RX-C-PLUS = if drop RX-TOK-PLUS dst cap out RX-EMIT-1 idx 1 + swap exit then
   dup RX-C-LBRACKET = if drop a u idx dst cap out RX-EMIT-CLASS swap exit then
   dup RX-UNSUPPORTED-META? if E-RX-SYNTAX throw then
   dst cap out RX-EMIT-LIT
   idx 1 + swap ;

: RX-COMPILE ( ptr u8 n ptr u8 n -- n ) {: a:ptr u dst:ptr cap :}
   u 0 < if E-RX-SYNTAX throw then
   cap 0 < if E-RX-CAPACITY throw then
   0 0 begin over u < while
      >r
      a u rot dst cap r> RX-SCAN-ONE
   repeat
   nip ;
