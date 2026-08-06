\ bootstrap-mirror-lint.f - tripwire for the gforth recovery corpus.
\
\ The no-binary recovery (tools/bootstrap.sh) compiles ONLY src/ sources
\ (SRC_COMMON: src/core + src/arch + src/os + src/habu) with the Gforth stage-0
\ emitter, which has NO width-aware pass-2 mirror (bootstrap/cg/forth.fs; dot
\ habu-bootstrap-mirror-pass-f1714953). That is sound exactly while no ADT
\ declaration (SUMTYPE / ENUM / PRODUCT / NEWTYPE) exists in src/ non-test
\ source: lib/'s wide families are compiled only by the recovered NATIVE engine
\ at the immediate fixpoint refresh. This lint turns that boundary into a red
\ gate: it fires the moment a declaration keyword appears as a live token in
\ src/, naming the mirror dot that must land first.
\
\ A live token is a lexed word (comments and strings skipped by the source
\ lexer) that is not a definition NAME (src/core/sumtype.f defines the words
\ SUMTYPE / ENUM / PRODUCT / NEWTYPE themselves) and not an escaped
\ reference (' / ['] / postpone / char / [char]).
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
   a u s" newtype" LINT-STR=CI ;

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

: WALK-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FORTH-FILE? 0= if exit then
   a u TEST-FILE? if exit then
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

\ ---- the recovery corpus is NOT all of src/ -----------------------------------
\ tools/bootstrap.sh names the stage-0 input explicitly, in one closed array
\ (SRC_COMMON), and every entry of it lies in one of the four directories below.
\ src/compiler is deliberately not among them: it is compiled by the RECOVERED
\ NATIVE engine at the immediate fixpoint refresh (bootstrap.sh runs
\ tools/build-fixpoint.f install --force straight after the stage), never by the
\ gforth stage-0 emitter, so a declaration there cannot reach the unmirrored
\ pass-2 and cannot carry a wide width fact through it.
\
\ Walking all of `src` scanned files the stage never sees, and when the compiler
\ packages landed their NEWTYPE/ENUM declarations that produced 56 findings about
\ a hazard that does not exist. Scanning the corpus instead is a narrowing of the
\ lint's DOMAIN to the invariant it exists to protect, not a loosening of its
\ rule: the rule below is unchanged and still fires on the first live declaration
\ keyword in any file the stage really compiles.
\
\ These four are a strict SUPERSET of SRC_COMMON - they hold files the array does
\ not name - so the scan can only over-report against the true corpus, never
\ under-report. What keeps that true as bootstrap.sh changes is CORPUS-DRIFT-CK
\ below, which reads the array itself.
4 constant ROOTS-N

: ROOT-AT ( n -- ptr u8 n ) {: i:n :}
   i 0 = if s" src/core" exit then
   i 1 = if s" src/arch" exit then
   i 2 = if s" src/os"   exit then
   s" src/habu" ;

: IN-CORPUS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   LINT-FALSE
   ROOTS-N 0 ?do
      a u i ROOT-AT LINT-PREFIX? if drop LINT-TRUE leave then
   loop ;

\ ---- the drift guard ----------------------------------------------------------
\ The four roots above restate a fact bootstrap.sh owns, and a restated fact
\ rots. This reads SRC_COMMON back and refuses any entry that lies outside them,
\ so adding a src/compiler file (or any new directory) to the stage-0 list turns
\ this lint red at once instead of silently taking that file out of the scan.
\ A path is an array entry that ends `.f` and is not a shell variable; the
\ `$OS_*` entries name files under src/os/macos and src/os/linux, both already
\ inside the src/os root, and they are checked by their expansion's directory
\ rather than by the variable's spelling.
$8000 constant BS-CAP
create BS-BUF BS-CAP allot

: BS-LINE-PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" LINT-SUFFIX? 0= if LINT-FALSE exit then
   a u s" /" LINT-CONTAINS? ;

: DRIFT-BAD ( ptr u8 n -- ) {: a:ptr u:n :}
   s" BOOTSTRAP-MIRROR tools/bootstrap.sh: SRC_COMMON entry `" type
   a u type
   s" ` lies outside the roots this lint scans; widen ROOT-AT, and mirror the pass-2 lowering first if it carries ADTs (dot habu-bootstrap-mirror-pass-f1714953)" type NL
   BAD-N @ 1+ BAD-N ! ;

variable BS-IN

: DRIFT-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LINT-TRIM {: t:ptr tu:n :}
   tu 0= if exit then
   t tu s" SRC_COMMON=(" LINT-CONTAINS? if 1 BS-IN ! exit then
   BS-IN @ 0= if exit then
   t tu s" )" LINT-PREFIX? if 0 BS-IN ! exit then
   t tu BS-LINE-PATH? 0= if exit then
   t tu IN-CORPUS? if exit then
   t tu DRIFT-BAD ;

: CORPUS-DRIFT-CK ( -- )
   0 BS-IN !
   s" tools/bootstrap.sh" BS-BUF BS-CAP READ-FILE {: a:ptr u:n :}
   a u SPLIT-LINES
   SN# @ 0 ?do i S@ DRIFT-LINE loop ;

: RUN ( -- )
   RESET
   ROOTS-N 0 ?do
      i ROOT-AT [: WALK-FILE ;] WALK-FILES
   loop
   CORPUS-DRIFT-CK
   FINISH ;

;package
