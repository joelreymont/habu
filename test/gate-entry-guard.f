\ Gate entry files are execution roots. A row may share an inert helper, but
\ importing or launching another row's entry silently runs that suite twice.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/test.f
require lib/fmt.f
require tools/lint/text.f
require tools/lint/source-lex.f

package ENTRY-GUARD
private

DYNAMIC-BUFFER PATH-BYTES u8
DYNAMIC-BUFFER PATH-OFF n
DYNAMIC-BUFFER PATH-LEN n
DYNAMIC-BUFFER ROOT-BYTES u8
DYNAMIC-BUFFER ROOT-OFF n
DYNAMIC-BUFFER ROOT-LEN n
DYNAMIC-BUFFER ROOT-ROW n
variable PATH-U
variable PATH-N
variable PATH-HEAD
variable ROOT-U
variable ROOT-N
variable OWNER
TYPED-VARIABLE CAND-A ptr u8
variable CAND-U
variable CAND-LINE
variable ERRORS
create TIER-CANON FS-PATH-CAP allot
variable TIER-U
create SOURCE-CANON FS-PATH-CAP allot
variable SOURCE-CANON-U

: PATH$ ( n -- ptr u8 n ) {: idx:n :}
   0 PATH-BYTES idx PATH-OFF @ +
   idx PATH-LEN @ ;

: PATH-RESET ( -- )
   0 PATH-U ! 0 PATH-N ! 0 PATH-HEAD ! ;

: PATH-SEEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   PATH-N @ 0 ?do
      a u i PATH$ STR= if true unloop exit then
   loop false ;

: PATH-ADD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u PATH-SEEN? if exit then
   PATH-U @ u + PATH-BYTES-RESERVE
   PATH-N @ 1+ PATH-OFF-RESERVE
   PATH-N @ 1+ PATH-LEN-RESERVE
   PATH-U @ PATH-N @ PATH-OFF !
   u PATH-N @ PATH-LEN !
   a 0 PATH-BYTES PATH-U @ + u BYTE-COPY
   PATH-U @ u + PATH-U !
   PATH-N @ 1+ PATH-N ! ;

: SOURCE$ ( -- ptr u8 n )
   PATH-HEAD @ PATH$ ;

\ This prefix selects tier 1 before its following suite file loads. Compare
\ physical identities so another spelling of it remains a prefix.
: TIER-INIT ( -- )
   s" test/compiler/aot-mode.f" SOURCE-ROOT:CANONICAL drop
   {: a:ptr u:n :}
   a TIER-CANON u BYTE-COPY u TIER-U ! ;

: TIER-PREFIX? ( ptr u8 n -- bool )
   SOURCE-ROOT:CANONICAL drop TIER-CANON TIER-U @ STR= ;

: ROOT$ ( n -- ptr u8 n ) {: idx:n :}
   0 ROOT-BYTES idx ROOT-OFF @ + idx ROOT-LEN @ ;

: ROOT-RESET ( -- )
   0 ROOT-U ! 0 ROOT-N ! ;

\ The registry's actual --load file arguments are frozen before scanning any
\ source. Canonical paths are stored once; candidate checks never resolve each
\ registered file again for every token.
: ROOT-ADD ( n ptr u8 n -- ) {: id:n path:ptr pathu:n :}
   path pathu SOURCE-ROOT:CANONICAL {: exists:bool :}
   exists 0= if
      s" entry guard: missing registered file: " type path pathu type cr
      E-SUITE-ROW throw
   then
   2dup TIER-CANON TIER-U @ STR= if 2drop exit then
   {: a:ptr u:n :}
   ROOT-U @ u + ROOT-BYTES-RESERVE
   ROOT-N @ 1+ ROOT-OFF-RESERVE
   ROOT-N @ 1+ ROOT-LEN-RESERVE
   ROOT-N @ 1+ ROOT-ROW-RESERVE
   ROOT-U @ ROOT-N @ ROOT-OFF !
   u ROOT-N @ ROOT-LEN !
   id ROOT-N @ ROOT-ROW !
   a 0 ROOT-BYTES ROOT-U @ + u BYTE-COPY
   ROOT-U @ u + ROOT-U !
   ROOT-N @ 1+ ROOT-N ! ;

: REPORT ( n -- ) {: id:n :}
   s" entry guard: " type SOURCE$ type
   s" :" type CAND-LINE @ FMT:.INT
   s" : row " type OWNER @ TEST:ITEM-NAME$ type
   s"  references registered entry " type id TEST:ITEM-NAME$ type
   s"  (" type CAND-A @ CAND-U @ type s" )" type cr
   1 ERRORS +! ;

: CHECK-PATH ( ptr u8 n n -- ) {: a:ptr u:n line:n :}
   a CAND-A ! u CAND-U ! line CAND-LINE !
   \ A non-path literal cannot name a registered load file.
   u 0 <= u FS-PATH-CAP >= or if exit then
   a u SOURCE-ROOT:CANONICAL {: exists:bool :}
   exists 0= if 2drop exit then
   {: canon:ptr canu:n :}
   canon canu SOURCE-CANON SOURCE-CANON-U @ STR= if exit then
   ROOT-N @ 0 ?do
      canon canu i ROOT$ STR= if
         i ROOT-ROW @ OWNER @ <> if i ROOT-ROW @ REPORT then
      then
   loop ;

: IMPORT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" require" STR=CI
   a u s" include" STR=CI or ;

: STRING? ( n -- bool ) {: idx:n :}
   idx LINT-LEX:TOKEN {: a:ptr u:n :}
   u 2 = if
      a c@ dup 115 = swap 83 = or a 1+ c@ 34 = and exit
   then
   u 3 <> if false exit then
   a c@ dup 115 = swap 83 = or
   a 1+ c@ 92 = and a 2 + c@ 34 = and ;

: STRING-IMPORT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" required" STR=CI
   a u s" included" STR=CI or ;

\ Comments are lexer tokens, but they leave the data stack untouched.
: NEXT-CODE ( n -- n )
   1+ begin dup LINT-LEX:COUNT < while
      dup LINT-LEX:KIND@ LINT-LEX:COMMENT <> if exit then
      1+
   repeat ;

\ These consumers take the path span directly. A later consumer is not proof
\ that an earlier string survived the words between them.
: DIRECT-LOAD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" GE-SRC-FILE+" STR=CI
   a u s" GE-ARG+" STR=CI or
   a u s" ARG" STR=CI or
   a u s" ARG+" STR=CI or
   a u s" ARGS" STR=CI or
   a u s" CHILD-RUN" STR=CI or ;

: DIAGNOSTIC-LOAD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" DIAGNOSTIC" STR=CI
   a u s" DIAGNOSTIC-WB" STR=CI or ;

: LITERAL-LOAD? ( n -- bool ) {: idx:n :}
   idx NEXT-CODE {: next:n :}
   next LINT-LEX:COUNT >= if false exit then
   next LINT-LEX:TOKEN DIRECT-LOAD? if true exit then
   next LINT-LEX:TOKEN s" >LEN" STR=CI if
      next NEXT-CODE {: consumer:n :}
      consumer LINT-LEX:COUNT >= if false exit then
      consumer LINT-LEX:TOKEN s" PROC-ARGV+" STR=CI exit
   then
   next STRING? if
      next NEXT-CODE {: consumer:n :}
      consumer LINT-LEX:COUNT >= if false exit then
      consumer LINT-LEX:TOKEN DIAGNOSTIC-LOAD? exit
   then
   false ;

: CHECK-IMPORT ( ptr u8 n n -- ) {: a:ptr u:n line:n :}
   a u line CHECK-PATH
   a u FILE? if a u PATH-ADD then ;

: SCAN-WORD ( n -- ) {: idx:n :}
   idx LINT-LEX:TOKEN IMPORT? 0= if exit then
   idx 1+ LINT-LEX:COUNT >= if exit then
   idx 1+ LINT-LEX:KIND@ LINT-LEX:WORD <> if exit then
   idx 1+ LINT-LEX:TOKEN
   idx 1+ LINT-LEX:LINE@ CHECK-IMPORT ;

: SCAN-STRING ( n -- ) {: idx:n :}
   idx STRING? 0= if exit then
   idx LINT-LEX:CONTENT {: a:ptr u:n :}
   idx LITERAL-LOAD? if a u idx LINT-LEX:LINE@ CHECK-PATH then
   idx NEXT-CODE {: next:n :}
   next LINT-LEX:COUNT >= if exit then
   next LINT-LEX:TOKEN STRING-IMPORT? if
      a u idx LINT-LEX:LINE@ CHECK-IMPORT
   then ;

: SCAN-TOKEN ( n -- ) {: idx:n :}
   idx LINT-LEX:KIND@ LINT-LEX:WORD <> if exit then
   idx SCAN-WORD
   idx SCAN-STRING ;

: SCAN-SOURCE ( -- )
   SOURCE$ SOURCE-ROOT:CANONICAL drop {: a:ptr u:n :}
   a SOURCE-CANON u BYTE-COPY u SOURCE-CANON-U !
   SOURCE$ LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT LINT-LEX:SOURCE
   LINT-LEX:ERROR? if
      s" entry guard: cannot lex " type SOURCE$ type cr
      E-SUITE-ROW throw
   then
   LINT-LEX:COUNT 0 ?do i SCAN-TOKEN loop ;

: CHECK-ENTRY ( n ptr u8 n -- ) {: id:n a:ptr u:n :}
   a u TIER-PREFIX? if exit then
   id OWNER !
   PATH-RESET
   a u PATH-ADD
   begin PATH-HEAD @ PATH-N @ < while
      SCAN-SOURCE
      PATH-HEAD @ 1+ PATH-HEAD !
   repeat ;

public

: CHECK ( -- )
   0 ERRORS !
   TIER-INIT
   ROOT-RESET
   [: ROOT-ADD ;] TEST:VISIT-LOAD-FILES
   [: CHECK-ENTRY ;] TEST:VISIT-LOAD-FILES
   ERRORS @ 0 > if E-SUITE-ROW throw then ;

;package
