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
require test/load-refs.f

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

: ROOT$ ( n -- ptr u8 n ) {: idx:n :}
   0 ROOT-BYTES idx ROOT-OFF @ + idx ROOT-LEN @ ;

: ROOT-RESET ( -- )
   0 ROOT-U ! 0 ROOT-N ! ;

\ The registry's actual --load file arguments are frozen before scanning any
\ source. Canonical paths are stored once; candidate checks never resolve each
\ registered file again for every token.
: ROOT-ADD ( n bool ptr u8 n -- ) {: id:n entry:bool path:ptr pathu:n :}
   path pathu SOURCE-ROOT:CANONICAL {: exists:bool :}
   exists 0= if
      s" entry guard: missing registered file: " type path pathu type cr
      E-SUITE-ROW throw
   then
   entry 0= if 2drop exit then
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

: REPORT-PRELOAD ( n -- ) {: id:n :}
   s" entry guard: row " type OWNER @ TEST:ITEM-NAME$ type
   s"  preloads registered entry " type id TEST:ITEM-NAME$ type
   s"  (" type CAND-A @ CAND-U @ type s" )" type cr
   1 ERRORS +! ;

: CHECK-PRELOAD ( ptr u8 n -- ) {: a:ptr u:n :}
   a CAND-A ! u CAND-U !
   a u SOURCE-ROOT:CANONICAL drop {: canon:ptr canu:n :}
   ROOT-N @ 0 ?do
      canon canu i ROOT$ STR= if
         i ROOT-ROW @ OWNER @ <> if i ROOT-ROW @ REPORT-PRELOAD then
      then
   loop ;

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

\ An import is followed as well as checked; a launch - a path literal a load
\ helper consumes - is only checked.
: REF ( ptr u8 n n bool -- ) {: a:ptr u:n line:n import:bool :}
   a u line CHECK-PATH
   import 0= if exit then
   a u FILE? if a u PATH-ADD then ;

: SCAN-SOURCE ( -- )
   SOURCE$ SOURCE-ROOT:CANONICAL drop {: a:ptr u:n :}
   a SOURCE-CANON u BYTE-COPY u SOURCE-CANON-U !
   SOURCE$ LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT LINT-LEX:SOURCE
   LINT-LEX:ERROR? if
      s" entry guard: cannot lex " type SOURCE$ type cr
      E-SUITE-ROW throw
   then
   [: REF ;] LOAD-REFS:EACH ;

: CHECK-FILE ( n bool ptr u8 n -- ) {: id:n entry:bool a:ptr u:n :}
   id OWNER !
   entry 0= if a u CHECK-PRELOAD then
   PATH-RESET
   a u PATH-ADD
   begin PATH-HEAD @ PATH-N @ < while
      SCAN-SOURCE
      PATH-HEAD @ 1+ PATH-HEAD !
   repeat ;

public

: CHECK ( -- )
   0 ERRORS !
   ROOT-RESET
   [: ROOT-ADD ;] TEST:VISIT-LOAD-FILES
   [: CHECK-FILE ;] TEST:VISIT-LOAD-FILES
   ERRORS @ 0 > if E-SUITE-ROW throw then ;

;package
