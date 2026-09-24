\ ir-id-source.f - read production emitter definitions for relocation replay.
\ The shared lexer keeps comments and strings out of the interpreted token stream.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/adt/option.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/source-lex.f
require tools/lint/def.f

package COMPILER-ID-SRC
private

variable HITS
variable FOUND
variable LIT-ACC

\ ---- token predicates --------------------------------------------------------

: WORD-TOK? ( n -- bool ) {: k:n :}
   k 0 < k LINT-LEX:COUNT >= or if false exit then
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

: TOK-IS? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD-TOK? 0= if false exit then
   k LINT-LEX:TOKEN a u STR= ;

: TOK-IS-CI? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD-TOK? 0= if false exit then
   k LINT-LEX:TOKEN a u STR=CI ;

\ ---- definitions -------------------------------------------------------------

: DEF-NAMES? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD-TOK? 0= if false exit then
   k LINT-DEF:DIRECT-KIND LINT-DEF:COLON <> if false exit then
   k LINT-DEF:NAME-I MATCH option
      none OF false ENDOF
      some OF a u TOK-IS? ENDOF
   ;MATCH ;

: DEF-BODY-START ( n -- n ) {: d:n :}
   d LINT-DEF:NAME-I MATCH option
      none OF E-CID-DEF throw ENDOF
      some OF 1+ ENDOF
   ;MATCH ;

: DEF-CLOSE ( n -- n ) {: b:n :}
   b
   begin dup LINT-LEX:COUNT < while
      dup LINT-DEF:COLON LINT-DEF:CLOSE? if exit then
      1+
   repeat
   drop E-CID-DEF throw ;

: DEF-INDEX ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1 FOUND !
   0 HITS !
   LINT-LEX:COUNT 0 ?do
      i a u DEF-NAMES? if
         HITS @ 1+ HITS !
         FOUND @ 0 < if i FOUND ! then
      then
   loop
   HITS @ 1 <> if E-CID-DEF throw then
   FOUND @ ;

\ ---- literals ----------------------------------------------------------------

: HEX-DIGIT ( n -- n ) {: c:n :}
   c $30 >= c $39 <= and if c $30 - exit then
   c $41 >= c $46 <= and if c $41 - 10 + exit then
   c $61 >= c $66 <= and if c $61 - 10 + exit then
   E-CID-CONST throw ;

: HEX@ ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 2 < u 17 > or if E-CID-CONST throw then
   0 LIT-ACC !
   u 1 ?do
      LIT-ACC @ 4 lshift a i + c@ HEX-DIGIT or LIT-ACC !
   loop
   LIT-ACC @ ;

: LIT@ ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0= if E-CID-CONST throw then
   a c@ $24 = if a u HEX@ exit then
   a u STR>NUMBER? MATCH option
      none OF E-CID-CONST throw ENDOF
      some OF ENDOF
   ;MATCH ;

: CONST-NAMES? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k s" constant" TOK-IS-CI? 0= if false exit then
   k 1+ a u TOK-IS? ;

public

\ ---- reading a source --------------------------------------------------------

: SCAN-TEXT ( ptr u8 n -- )
   LINT-LEX:SOURCE
   LINT-LEX:ERROR? if E-CID-LEX throw then ;

: SCAN-FILE ( ptr u8 n -- )
   LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT SCAN-TEXT ;

: DEF-HEAD? ( n -- bool ) {: k:n :}
   k WORD-TOK? 0= if false exit then
   k LINT-DEF:DIRECT-KIND LINT-DEF:COLON <> if false exit then
   k LINT-DEF:NAME-I MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

\ The half-open token range of that definition's body: the token after the name
\ up to, and not including, its closer.
: DEF-SPAN-AT ( n -- n n ) {: k:n :}
   k DEF-HEAD? 0= if E-CID-DEF throw then
   k DEF-BODY-START {: b:n :}
   b b DEF-CLOSE ;

\ The same range for the definition named exactly once by this word.
: BODY-SPAN ( ptr u8 n -- n n )
   DEF-INDEX DEF-SPAN-AT ;

\ The literal a `<literal> constant NAME` row carries, as a number.
: CONST@ ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1 FOUND !
   0 HITS !
   LINT-LEX:COUNT 0 ?do
      i a u CONST-NAMES? if
         HITS @ 1+ HITS !
         FOUND @ 0 < if i FOUND ! then
      then
   loop
   HITS @ 1 <> if E-CID-CONST throw then
   FOUND @ 1 < if E-CID-CONST throw then
   FOUND @ 1- WORD-TOK? 0= if E-CID-CONST throw then
   FOUND @ 1- LINT-LEX:TOKEN LIT@ ;

\ ---- raw token access --------------------------------------------------------
\ The relocation interpreter shares the lexer's token stream.

: WORD-TOKEN? ( n -- bool )
   WORD-TOK? ;

: TOKEN$ ( n -- ptr u8 n ) {: k:n :}
   k WORD-TOK? 0= if s" " exit then
   k LINT-LEX:TOKEN ;

;package
