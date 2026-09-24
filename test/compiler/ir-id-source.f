\ ir-id-source.f - structural source reader for native compiler schema tests.
\ The shared lexer and definer registry keep comments and strings out of guards.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/adt/option.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/lint/def.f

package COMPILER-ID-SRC
private

$800 constant RUN-CAP

create RUN-BUF RUN-CAP allot

variable RUN-U
variable FLD-LEN
variable EXP-OFF
variable EXP-K
variable HITS
variable FOUND
variable LIT-ACC

\ ---- normalized token-run buffer ---------------------------------------------

: RUN-RESET ( -- )
   0 RUN-U ! ;

: RUN+C ( n -- ) {: c:n :}
   RUN-U @ 1+ RUN-CAP > if E-STR-CAPACITY throw then
   c RUN-BUF RUN-U @ + c!
   RUN-U @ 1+ RUN-U ! ;

: RUN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   RUN-U @ u + RUN-CAP > if E-STR-CAPACITY throw then
   a RUN-BUF RUN-U @ + u BYTE-COPY
   RUN-U @ u + RUN-U ! ;

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

\ The next word token at or after k; LINT-LEX:COUNT when there is none. Comment
\ tokens are stepped over, so a comment cannot break a token run apart and a
\ comment's own bytes can never be matched as code.
: WORD-K ( n -- n ) {: k:n :}
   k 0 < if 0 exit then
   k
   begin dup LINT-LEX:COUNT < while
      dup WORD-TOK? if exit then
      1+
   repeat ;

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

: STORE-NAMES? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k a u TOK-IS? 0= if false exit then
   k 1+ s" !" TOK-IS? ;

public

\ ---- reading a source --------------------------------------------------------

: SCAN-TEXT ( ptr u8 n -- )
   LINT-LEX:SOURCE
   LINT-LEX:ERROR? if E-CID-LEX throw then ;

: SCAN-FILE ( ptr u8 n -- )
   LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT SCAN-TEXT ;

\ ---- structural answers ------------------------------------------------------

\ How many `:` definitions name this word. The gate asserts one; a hostile
\ duplicate or a name that only appears inside a comment or a string answers a
\ different number instead of silently resolving to the first hit.
: DEFS ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 HITS !
   LINT-LEX:COUNT 0 ?do
      i a u DEF-NAMES? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

\ The named definition's body as its word tokens joined by one space. Stack
\ effect and interior comments are not part of it; a string literal contributes
\ its opener only, because the lexer never turns quoted bytes into tokens.
: BODY$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u DEF-INDEX DEF-BODY-START {: b:n :}
   b DEF-CLOSE {: e:n :}
   RUN-RESET
   e b ?do
      i WORD-TOK? if
         RUN-U @ 0 > if $20 RUN+C then
         i LINT-LEX:TOKEN RUN+
      then
   loop
   RUN-BUF RUN-U @ ;

\ ---- definition spans --------------------------------------------------------
\ The same walk `BODY$` uses, exposed as token indices instead of joined bytes.
\ A consumer that has to reason about the ORDER of tokens inside one definition -
\ "every capacity check runs before the first arena push" - cannot ask that of a
\ joined string without falling back to substring search, so it asks for the span
\ and reads the shared lexer's tokens through `TOKEN$`.

: DEF-HEAD? ( n -- bool ) {: k:n :}
   k WORD-TOK? 0= if false exit then
   k LINT-DEF:DIRECT-KIND LINT-DEF:COLON <> if false exit then
   k LINT-DEF:NAME-I MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

\ The name the definition opening at k gives its word.
: DEF-NAME-AT$ ( n -- ptr u8 n ) {: k:n :}
   k DEF-HEAD? 0= if E-CID-DEF throw then
   k LINT-DEF:NAME-I MATCH option
      none OF E-CID-DEF throw ENDOF
      some OF LINT-LEX:TOKEN ENDOF
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

: CONSTS ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 HITS !
   LINT-LEX:COUNT 0 ?do
      i a u CONST-NAMES? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

\ How many `<literal> constant NAME` rows give their word a name beginning with
\ this prefix. A consumer that has frozen a whole naming FAMILY - every `CC-`
\ code the checker's concrete type table can name - asks this, because pinning
\ each member one at a time says nothing about a member that was added.
: CONSTS-PREFIXED ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 HITS !
   LINT-LEX:COUNT 1- 0 ?do
      i s" constant" TOK-IS-CI? if
         i 1+ WORD-TOK? if
            i 1+ LINT-LEX:TOKEN a u STARTS-WITH? if HITS @ 1+ HITS ! then
         then
      then
   loop
   HITS @ ;

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

\ The literal a `<literal> NAME !` row writes, for the one row that names it.
: STORE@ ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1 FOUND !
   0 HITS !
   LINT-LEX:COUNT 0 ?do
      i a u STORE-NAMES? if
         HITS @ 1+ HITS !
         FOUND @ 0 < if i FOUND ! then
      then
   loop
   HITS @ 1 <> if E-CID-CONST throw then
   FOUND @ 1 < if E-CID-CONST throw then
   FOUND @ 1- WORD-TOK? 0= if E-CID-CONST throw then
   FOUND @ 1- LINT-LEX:TOKEN LIT@ ;

\ ---- raw token access --------------------------------------------------------
\ Schema cases share the lexer's token stream instead of rereading the source.

: TOKENS ( -- n )
   LINT-LEX:COUNT ;

: WORD-TOKEN? ( n -- bool )
   WORD-TOK? ;

: TOKEN$ ( n -- ptr u8 n ) {: k:n :}
   k WORD-TOK? 0= if s" " exit then
   k LINT-LEX:TOKEN ;

\ What a token CARRIES rather than what it spells: a string literal's payload,
\ a paren comment's body. The shared lexer never turns those bytes into tokens,
\ so a consumer that has to read a quoted NAME out of production source - the
\ checker writes its concrete type table as `s" n" CC-N ... CT-SET` - reaches it
\ only here, and never by searching the source text for the spelling.
: TOKEN-CONTENT$ ( n -- ptr u8 n ) {: k:n :}
   k 0 < k LINT-LEX:COUNT >= or if s" " exit then
   k LINT-LEX:CONTENT ;

\ Does the word-token stream carry this exact space-separated run starting at
\ token k?
: RUN-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n start:n :}
   0 EXP-OFF !
   start WORD-K EXP-K !
   begin EXP-OFF @ u < while
      0 FLD-LEN !
      begin
         EXP-OFF @ FLD-LEN @ + u <
         if a EXP-OFF @ FLD-LEN @ + + c@ $20 <> else false then
      while
         FLD-LEN @ 1+ FLD-LEN !
      repeat
      FLD-LEN @ 0= if
         EXP-OFF @ 1+ EXP-OFF !
      else
         EXP-K @ a EXP-OFF @ + FLD-LEN @ TOK-IS? 0= if false exit then
         EXP-OFF @ FLD-LEN @ + EXP-OFF !
         EXP-K @ 1+ WORD-K EXP-K !
      then
   repeat
   true ;

\ How many times that run occurs. The gate asserts one for each frozen run, so
\ deleting it, duplicating it, or reordering its tokens all fail.
: RUNS ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 HITS !
   LINT-LEX:COUNT 0 ?do
      i WORD-TOK? if
         a u i RUN-AT? if HITS @ 1+ HITS ! then
      then
   loop
   HITS @ ;

;package
