\ bootstrap-mirror-lint.f - tripwire for the gforth recovery corpus.
\
\ The no-binary recovery (tools/bootstrap.sh) compiles ONLY src/ sources
\ (SRC_COMMON: src/core + src/arch + src/os + src/habu) with the Gforth stage-0
\ emitter, which has NO width-aware pass-2 mirror (bootstrap/cg/forth.fs; dot
\ habu-bootstrap-mirror-pass-f1714953). That is sound exactly while no ADT
\ declaration (SUMTYPE / ENUM / PRODUCT / TYPEFAMILY) exists in src/ non-test
\ source: lib/'s wide families are compiled only by the recovered NATIVE engine
\ at the immediate fixpoint refresh. This lint turns that boundary into a red
\ gate: it fires the moment a declaration keyword appears as a live token in
\ src/, naming the mirror dot that must land first.
\
\ A live token is a lexed word (comments and strings skipped by the source
\ lexer) that is not a definition NAME (src/core/sumtype.f defines the words
\ SUMTYPE / ENUM / PRODUCT / TYPEFAMILY themselves) and not an escaped
\ reference (' / ['] / postpone / char / [char]).
\
\ Native-engine-only src subtrees that the gforth stage-0 never compiles (they are
\ loaded by the recovered native engine only, like lib/) are OUTSIDE this corpus
\ and skipped: src/cad (the Model-CAD layer over lib/nominal) carries the finite
\ effect-atom / slot-kind sums and the effect-row family, none of which reach the
\ Gforth emitter (they are absent from bootstrap.sh SRC_COMMON and srclist.f).
\
\ This is a standalone entry point: it loads its own dependencies, so
\ `bin/hb --load tools/bootstrap-mirror-lint.f` works on its own. It used to only
\ list them in a comment and rely on the caller having loaded them first, which
\ held inside test/run.f but left the standalone command dead.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

package BOOTSTRAP-MIRROR-LINT

$20 constant NUM-CAP
$0A constant LF

create NUM NUM-CAP allot
create LF-BUF 1 allot

variable SRC-A
variable SRC-U
variable SRC-CAP
variable FILE-A
variable FILE-U
variable BAD-N
variable FILE-N
variable SCAN-I
variable NUM-I

: SRC-A-FIELD ( -- ptr ptr u8 )
   SRC-A 0 ptr-field ;

: FILE-A-FIELD ( -- ptr ptr u8 )
   FILE-A 0 ptr-field ;

: SRC-A@ ( -- ptr u8 )
   SRC-A-FIELD @ ;

: SRC-A! ( ptr u8 -- )
   SRC-A-FIELD ! ;

: FILE-A@ ( -- ptr u8 )
   FILE-A-FIELD @ ;

: FILE-A! ( ptr u8 -- )
   FILE-A-FIELD ! ;

: C ( n -- )
   LF-BUF c!
   LF-BUF 1 type ;

: NL ( -- )
   LF C ;

: U. ( n -- ) {: u:n :}
   NUM-CAP NUM-I !
   u 0= if
      NUM-I @ 1- NUM-I !
      $30 NUM NUM-I @ + c!
      NUM NUM-I @ + 1 type
      exit
   then
   u begin dup 0 > while
      dup 10 mod $30 +
      NUM-I @ 1- NUM-I !
      NUM NUM-I @ + c!
      10 /
   repeat drop
   NUM NUM-I @ + NUM-CAP NUM-I @ - type ;

: KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" sumtype" LINT-STR=CI if LINT-TRUE exit then
   a u s" enum" LINT-STR=CI if LINT-TRUE exit then
   a u s" product" LINT-STR=CI if LINT-TRUE exit then
   a u s" typefamily" LINT-STR=CI ;

: WORD? ( n -- bool ) {: k:n :}
   k LINT-LEX:COUNT >= if LINT-FALSE exit then
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

: NAME-POSITION? ( n -- bool ) {: k:n :}   \ token k is a definition NAME, not a declaration
   k 0 <= if LINT-FALSE exit then
   k 1- WORD? 0= if LINT-FALSE exit then
   k 1- LINT-LEX:TOKEN s" :" LINT-STR= if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" +:" LINT-STR= if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" TRUSTED:" LINT-STR=CI if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" KERNEL:" LINT-STR=CI if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" PRIM:" LINT-STR=CI if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" create" LINT-STR=CI if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" variable" LINT-STR=CI if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" constant" LINT-STR=CI if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" defer" LINT-STR=CI ;

: ESCAPED? ( n -- bool ) {: k:n :}         \ token k is a quoted/parsed reference
   k 0 <= if LINT-FALSE exit then
   k 1- WORD? 0= if LINT-FALSE exit then
   k 1- LINT-LEX:TOKEN s" '" LINT-STR= if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" [']" LINT-STR= if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" postpone" LINT-STR=CI if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" char" LINT-STR=CI if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" [char]" LINT-STR=CI ;

: REPORT ( n -- ) {: k:n :}
   BAD-N @ 1+ BAD-N !
   s" BOOTSTRAP-MIRROR " type
   FILE-A@ FILE-U @ type
   $3A C k LINT-LEX:LINE@ U.
   s" : ADT declaration `" type k LINT-LEX:TOKEN type
   s" ` in the gforth-compiled recovery corpus; land the stage-0 pass-2 mirror first (dot habu-bootstrap-mirror-pass-f1714953)" type
   NL ;

: SCAN-TOKEN ( -- )
   SCAN-I @ WORD? 0= if exit then
   SCAN-I @ LINT-LEX:TOKEN KEYWORD? 0= if exit then
   SCAN-I @ NAME-POSITION? if exit then
   SCAN-I @ ESCAPED? if exit then
   SCAN-I @ REPORT ;

: SCAN ( -- )
   0 SCAN-I !
   begin SCAN-I @ LINT-LEX:COUNT < while
      SCAN-TOKEN
      SCAN-I @ 1+ SCAN-I !
   repeat ;

: ALLOC-NEED ( n -- n ) {: n:n :}
   n 0 <= if 1 exit then
   n ;

: LOAD-SOURCE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE ALLOC-NEED MEM-ALLOC-64K-SPAN SRC-CAP ! SRC-A!
   path pathu SRC-A@ SRC-CAP @ READ-ALL SRC-U ! ;

public

: FILE-AS ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n label:ptr labelu:n :}
   label FILE-A!  labelu FILE-U !
   path pathu LOAD-SOURCE
   FILE-N @ 1+ FILE-N !
   SRC-A@ SRC-U @ LINT-LEX:SOURCE
   SCAN ;

: FILE ( ptr u8 n -- )
   2dup FILE-AS ;

private

: FORTH-FILE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" HAS-EXT? if LINT-TRUE exit then
   a u s" .fs" HAS-EXT? ;

: TEST-FILE? ( ptr u8 n -- bool )          \ src/ carries no test sources today; skip any that appear
   s" test" LINT-CONTAINS? ;

: NATIVE-ONLY? ( ptr u8 n -- bool )        \ native-engine-only src subtrees outside the gforth SRC_COMMON corpus
   s" src/cad/" LINT-CONTAINS? ;               \ CAD model layer: consumes lib/nominal, loaded only by the native engine

: WALK-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FORTH-FILE? 0= if exit then
   a u TEST-FILE? if exit then
   a u NATIVE-ONLY? if exit then
   a u FILE ;

public

: RESET ( -- )
   0 BAD-N !
   0 FILE-N ! ;

: FINISH ( -- )
   s" bootstrap-mirror-lint: " type
   FILE-N @ U. s"  file(s), " type
   BAD-N @ U. s"  finding(s)" type NL
   BAD-N @ 0 > if 1 throw then ;

: RUN ( -- )
   RESET
   s" src" [: WALK-FILE ;] WALK-FILES
   FINISH ;

;package
