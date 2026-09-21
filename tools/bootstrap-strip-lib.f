\ bootstrap-strip-lib.f - remove only source lines the engine's prefix reader
\ would discard, while consulting the shared lexer before dropping a line that
\ starts inside a string, paren comment, or primitive row.
\
\ The source reader drops a line whose first token is a standalone `\`, and
\ blank lines. A source lexer token can span that byte position, so the raw
\ line rule alone is unsafe for the baked compiler source.

require lib/errors.f
require lib/string.f
require tools/lint/source-lex.f

package BOOTSTRIP
private

variable LINE-P
variable TOK-I
variable OUT-U

: LINE-NEXT ( ptr u8 n n -- n ) {: a:ptr u:n p:n :}
   p begin dup u < while
      dup a + c@ 10 = if 1+ exit then
      1+
   repeat ;

: FIRST-TOKEN-BYTE ( ptr u8 n n -- n ) {: a:ptr u:n p:n :}
   p begin dup u < while
      dup a + c@ 10 = if exit then
      dup a + c@ 32 > if exit then
      1+
   repeat ;

: DROP-LINE? ( ptr u8 n n -- bool ) {: a:ptr u:n p:n :}
   a u p FIRST-TOKEN-BYTE {: i:n :}
   i u >= if true exit then
   i a + c@ 10 = if true exit then
   i a + c@ 92 <> if false exit then
   i 1+ u >= if true exit then
   i 1+ a + c@ 32 <= ;

\ A string token's CONTENT is its payload, so its end is the closing quote
\ rather than the end of the opener word. Registry and paren tokens already
\ span their complete inert body.
: TOKEN-END ( n -- n ) {: k:n :}
   k LINT-LEX:TOKEN {: ta:ptr tu:n :}
   k LINT-LEX:BYTE@ tu + {: e:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD <> if e exit then
   k LINT-LEX:CONTENT {: ca:ptr cu:n :}
   cu 0= if e exit then
   e cu + 2 + ;

: INSIDE-TOKEN? ( n -- bool ) {: p:n :}
   begin TOK-I @ LINT-LEX:COUNT < while
      TOK-I @ TOKEN-END p > if
         TOK-I @ LINT-LEX:BYTE@ p < exit
      then
      TOK-I @ 1+ TOK-I !
   repeat
   false ;

: COPY-LINE ( ptr u8 n n ptr u8 n -- n )
   {: a:ptr u:n p:n dst:ptr cap:n :}
   a u p LINE-NEXT {: next:n :}
   next p - {: len:n :}
   a p + dst OUT-U @ + len BYTE-COPY
   OUT-U @ len + OUT-U !
   next ;

public

: STRIP-BYTES ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n dst:ptr cap:n :}
   cap u < if s" bootstrap-strip: output is too small" 74 die then
   a u LINT-LEX:SOURCE
   LINT-LEX:ERROR? if s" bootstrap-strip: malformed source" 74 die then
   0 OUT-U ! 0 TOK-I ! 0 LINE-P !
   begin LINE-P @ u < while
      a u LINE-P @ DROP-LINE? if
         LINE-P @ INSIDE-TOKEN? if
            a u LINE-P @ dst cap COPY-LINE LINE-P !
         else
            a u LINE-P @ LINE-NEXT LINE-P !
         then
      else
         a u LINE-P @ dst cap COPY-LINE LINE-P !
      then
   repeat
   OUT-U @ ;

;package
