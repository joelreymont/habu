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
using LINT-SPLIT

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

: LEX-FILE ( ptr u8 n -- )                 \ read one source file and lex it
   LOAD-SOURCE
   SRC-A@ SRC-U @ LINT-LEX:SOURCE ;

public

: FILE-AS ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n label:ptr labelu:n :}
   label FILE-A!  labelu FILE-U !
   path pathu LEX-FILE
   FILE-N @ 1+ FILE-N !
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

\ ---- every boot-prefix row reaches the seed ------------------------------------
\ tools/boot-pin.f BP-EACH owns the list of sources the engine re-reads at boot.
\ The recovery seed has to COMPILE every one of them: a row no seed list names is
\ a word the stage never defines, and the Gforth-built hb-stage0 dies at its first
\ call with the bare token on stderr and exit 70. Measured 2026-08-22 on this
\ tree: src/core/bytes.f absent died `BYTE-COPY` (src/habu/aot-ident.f PATH+),
\ and with that row added src/core/include.f absent died `REQUIRE-REG:COUNT`
\ (src/core/lower-cert-seal.f). Nothing checked this before -- the drift guard
\ above reads SRC_COMMON only to ask which directory each entry lies in.
\
\ ORDER IS NOT CHECKED, deliberately. boot-pin's order is the engine's boot
\ RELOAD order; the seed's is one Gforth COMPILE order, and the two differ by
\ construction (tools/build-fixpoint.f keeps them apart as BF-APPEND-BOOT-CORE
\ against BF-APPEND-COMMON; on this tree src/core/include.f and
\ src/os/script-argv.f sit in different relative places in each). The seed's own
\ order needs no lint: a use before its definition is refused by the recovery
\ build itself, loudly, on every run.
$24 constant DOLLAR
$3B constant SEMI

$8000 constant SEED-CAP                    \ tools/bootstrap.sh is ~16K today
create SEED-BUF SEED-CAP allot
$80 constant SEED-MAX                      \ seed rows; ~64 today
create SEED-ROW-A SEED-MAX cells allot
create SEED-ROW-U SEED-MAX cells allot
variable SEED-N
variable SEED-TEXT-U
variable SEED-I                            \ line cursor into the script text
variable TEXT-A
variable IN-ARRAY
variable IN-EMIT
variable IF-DEPTH                          \ conditional nesting inside emit_src
variable ARRAY-USED                        \ emit_src really expands SRC_COMMON

: TEXT-A-FIELD ( -- ptr ptr u8 )
   TEXT-A 0 ptr-field ;

: TEXT-A@ ( -- ptr u8 )
   TEXT-A-FIELD @ ;

: SEED-A-FIELD ( n -- ptr ptr u8 )
   SEED-ROW-A swap ptr-field ;

: SEED-ROW+ ( ptr u8 n -- )                \ one source path the seed compiles
   SEED-N @ SEED-MAX >= if
      s" bootstrap-mirror-lint: seed row table full" 1 die
   then
   SEED-N @ cells SEED-ROW-U + !
   SEED-N @ SEED-A-FIELD !
   SEED-N @ 1+ SEED-N ! ;

: SEED-ROW$ ( n -- ptr u8 n ) {: i:n :}
   i SEED-A-FIELD @
   i cells SEED-ROW-U + @ ;

: SEED-HAS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup SEED-N @ < while
      dup SEED-ROW$ a u LINT-STR= if drop LINT-TRUE exit then
      1 +
   repeat
   drop LINT-FALSE ;

\ Neither list spells the two target-selected sources: the script reaches them
\ through the $OS_* variables its `case` sets, boot-pin through the BP-OS-*$
\ words. Both sides normalise to one key, so those rows still take part.
: OS-TARGET-KEY ( -- ptr u8 n )
   s" src/os/*/target.f" ;

: OS-LAYOUT-KEY ( -- ptr u8 n )
   s" src/os/*/layout.f" ;

\ ---- the script's two lists, read by field role --------------------------------
: FIELD= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k SN# @ >= if LINT-FALSE exit then
   k S@ a u LINT-STR= ;

: PEEK-LAST ( ptr u8 n -- ptr u8 n n )     \ last byte of a non-empty span
   2dup 1 - + c@ ;

: UNQUOTE ( ptr u8 n -- ptr u8 n )         \ a shell word without its "..."
   dup 2 < if exit then
   over c@ DQUOTE <> if exit then
   PEEK-LAST DQUOTE <> if exit then
   swap 1 + swap 2 - ;

: STRIP-SEMI ( ptr u8 n -- ptr u8 n )      \ a shell word without its trailing `;`
   dup 0= if exit then
   PEEK-LAST SEMI <> if exit then
   1 - ;

: SHELL-VAR? ( ptr u8 n -- bool )
   dup 0= if 2drop LINT-FALSE exit then
   drop c@ DOLLAR = ;

: OS-VAR-ROW ( ptr u8 n -- )               \ the two keyed $OS_* entries; the rest name no boot-pin row
   2dup s" $OS_TARGET" LINT-STR= if 2drop OS-TARGET-KEY SEED-ROW+ exit then
   2dup s" $OS_LAYOUT" LINT-STR= if 2drop OS-LAYOUT-KEY SEED-ROW+ exit then
   2drop ;

: PATH-ROW+ ( ptr u8 n -- )                \ a shell word, kept only if it names a source
   2dup SHELL-VAR? if OS-VAR-ROW exit then
   2dup BS-LINE-PATH? 0= if 2drop exit then
   SEED-ROW+ ;

: ARRAY-LINE ( -- )                        \ one line of the SRC_COMMON array
   SN# @ 1 <> if exit then
   0 S@ UNQUOTE PATH-ROW+ ;

\ `cat <path> >> "$out"` is the only row shape emit_src writes. Every field is
\ matched by its ROLE, so a path in a comment (`#` leads), inside a string
\ (`printf` leads), or with another redirect target is not a row.
: CAT-ROW? ( -- bool )
   SN# @ 4 <> if LINT-FALSE exit then
   0 s" cat" FIELD= 0= if LINT-FALSE exit then
   2 s" >>" FIELD= 0= if LINT-FALSE exit then
   3 S\" \"$out\"" FIELD= ;

\ The array reaches a stage source only where emit_src expands it; bash splices
\ the entries in at this line, so without it no array entry is compiled at all.
: ARRAY-EXPANDS? ( -- bool )
   SN# @ 5 <> if LINT-FALSE exit then
   0 s" for" FIELD= 0= if LINT-FALSE exit then
   3 S@ STRIP-SEMI S\" \"${SRC_COMMON[@]}\"" LINT-STR= ;

: IF-OPEN? ( -- bool )
   0 s" if" FIELD= ;

: IF-CLOSE? ( -- bool )
   SN# @ 1 <> if LINT-FALSE exit then
   0 s" fi" FIELD= ;

\ A cat row inside a conditional is NOT in the seed. Every emission is a full
\ engine that re-reads the whole boot prefix, so a boot-prefix row has to be in
\ EVERY emission, and a guarded row is in at most one. Measured: src/core/
\ include.f sat behind `if [[ "$driver" == "src/habu/snap.f" ]]`, a branch no
\ emit_src call site could select, and hb-stage0 died `REQUIRE-REG:COUNT`.
: EMIT-LINE ( -- )                         \ one line of emit_src
   IF-OPEN? if IF-DEPTH @ 1+ IF-DEPTH ! exit then
   IF-CLOSE? if IF-DEPTH @ 1- IF-DEPTH ! exit then
   IF-DEPTH @ 0= 0= if exit then
   ARRAY-EXPANDS? if 1 ARRAY-USED ! exit then
   CAT-ROW? 0= if exit then
   1 S@ UNQUOTE PATH-ROW+ ;

\ The four openers and closers are compared against the WHOLE line, so they hold
\ only at column 0 where the script writes them; an indented look-alike is not a
\ boundary, and a script that stops matching collects no rows and fails closed.
: SCRIPT-LINE ( ptr u8 n -- )
   2dup s" SRC_COMMON=(" LINT-STR= if 2drop 1 IN-ARRAY ! exit then
   2dup s" emit_src() {" LINT-STR= if 2drop 1 IN-EMIT ! 0 IF-DEPTH ! exit then
   IN-ARRAY @ 0= 0= if
      2dup s" )" LINT-STR= if 2drop 0 IN-ARRAY ! exit then
      SPLIT-WHITESPACE ARRAY-LINE exit
   then
   IN-EMIT @ 0= 0= if
      2dup s" }" LINT-STR= if 2drop 0 IN-EMIT ! exit then
      SPLIT-WHITESPACE EMIT-LINE exit
   then
   2drop ;

\ Lines are walked on this package's own cursor: LINT-SPLIT holds ONE result
\ table, and the field split of each line would overwrite a line split.
: LINE-END ( -- n )                        \ offset of the LF ending the line at the cursor
   SEED-I @
   begin dup SEED-TEXT-U @ < while
      TEXT-A@ over + c@ LF = if exit then
      1 +
   repeat ;

: NEXT-LINE ( -- ptr u8 n )                \ the line at the cursor; cursor moves past it
   LINE-END {: e:n :}
   TEXT-A@ SEED-I @ +
   e SEED-I @ -
   e 1 + SEED-I ! ;

: SEED-SOURCES ( ptr u8 n -- )             \ a bootstrap.sh -> the set of sources it compiles
   0 SEED-N !  0 IN-ARRAY !  0 IN-EMIT !  0 IF-DEPTH !  0 ARRAY-USED !
   SEED-BUF SEED-CAP READ-FILE SEED-TEXT-U ! TEXT-A-FIELD !
   0 SEED-I !
   begin SEED-I @ SEED-TEXT-U @ < while
      NEXT-LINE SCRIPT-LINE
   repeat ;

\ ---- boot-pin's list, read through the shared source lexer ---------------------
\ boot-pin.f spells the list once, in BP-EACH. The walk is bounded by that one
\ definition and reads lexer tokens, so a path in a comment, in another word, or
\ in a diagnostic string is not a row.
: TOKEN= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD? 0= if LINT-FALSE exit then
   k LINT-LEX:TOKEN a u LINT-STR=CI ;

: STRING-TOKEN? ( n -- bool ) {: k:n :}
   k WORD? 0= if LINT-FALSE exit then
   k LINT-LEX:TOKEN LINT-NORMAL-STRING-OPENER? if LINT-TRUE exit then
   k LINT-LEX:TOKEN LINT-ESC-STRING-OPENER? ;

: PIN-BODY-START ( -- n )                  \ token index after `: BP-EACH`, or -1
   0 begin dup LINT-LEX:COUNT 1 - < while
      dup s" :" TOKEN= over 1 + s" BP-EACH" TOKEN= and if 2 + exit then
      1 +
   repeat
   drop -1 ;

variable PIN-I
variable PIN-DEPTH

: QUOT-DELTA ( n -- n ) {: k:n :}          \ quotation nesting change at token k
   k s" [:" TOKEN= if 1 exit then
   k s" ;]" TOKEN= if -1 exit then
   0 ;

: PIN-ROW? ( n -- bool ) {: k:n :}
   k STRING-TOKEN? if LINT-TRUE exit then
   k s" BP-OS-TARGET$" TOKEN= if LINT-TRUE exit then
   k s" BP-OS-LAYOUT$" TOKEN= ;

: PIN-ROW$ ( n -- ptr u8 n ) {: k:n :}
   k s" BP-OS-TARGET$" TOKEN= if OS-TARGET-KEY exit then
   k s" BP-OS-LAYOUT$" TOKEN= if OS-LAYOUT-KEY exit then
   k LINT-LEX:CONTENT ;

\ Only src/ rows: the lib/ prefix rows are compiled by the recovered NATIVE
\ engine at the fixpoint refresh, never by the gforth stage, which is this
\ lint's founding premise.
: SRC-ROW? ( ptr u8 n -- bool )
   s" src/" LINT-PREFIX? ;

: PIN-EACH ( [ ptr u8 n -- ] -- )          \ every src/ row BP-EACH names, in order
   {: q :}  \ typed-local-lint: allow-bare-local - quotation bound as ordinary local (docs/forth.md)
   PIN-BODY-START PIN-I !
   PIN-I @ 0 < if exit then
   0 PIN-DEPTH !
   begin PIN-I @ LINT-LEX:COUNT < while
      PIN-I @ QUOT-DELTA PIN-DEPTH @ + PIN-DEPTH !
      PIN-I @ s" ;" TOKEN= PIN-DEPTH @ 0= and if exit then
      PIN-I @ PIN-ROW? if
         PIN-I @ PIN-ROW$ 2dup SRC-ROW? if q execute else 2drop then
      then
      PIN-I @ 1 + PIN-I !
   repeat ;

variable WANT-A
variable WANT-U
variable WANT-HIT

: WANT-A-FIELD ( -- ptr ptr u8 )
   WANT-A 0 ptr-field ;

: WANT$ ( -- ptr u8 n )
   WANT-A-FIELD @ WANT-U @ ;

: WANT-MATCH ( ptr u8 n -- )
   WANT$ LINT-STR= if 1 WANT-HIT ! then ;

: PIN-HAS? ( ptr u8 n -- bool )            \ does BP-EACH name this source
   WANT-U !  WANT-A-FIELD !
   0 WANT-HIT !
   [: WANT-MATCH ;] PIN-EACH
   WANT-HIT @ 0= 0= ;

\ ---- the exemptions ------------------------------------------------------------
\ Boot-prefix rows the seed source deliberately does not compile, each with the
\ reason it is exempt. One row today, so the index falls through to it; a second
\ takes an `i 0 = if ... exit then` arm above, exactly as ROOT-AT does. A stale
\ exemption is itself a finding, so the table cannot grow into a graveyard.
1 constant EXEMPT-N

: EXEMPT-PATH$ ( n -- ptr u8 n )
   drop s" src/core/internal-mark.f" ;

: EXEMPT-WHY$ ( n -- ptr u8 n )
   drop s" no seed source calls anything it defines; the engine loads it at boot from the checkout" ;

: EXEMPT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup EXEMPT-N < while
      dup EXEMPT-PATH$ a u LINT-STR= if drop LINT-TRUE exit then
      1 +
   repeat
   drop LINT-FALSE ;

\ ---- findings ------------------------------------------------------------------
: MISSING-ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   BAD-N @ 1+ BAD-N !
   s" BOOTSTRAP-MIRROR tools/bootstrap.sh: boot-prefix source `" type
   a u type
   s" ` is named by tools/boot-pin.f but no seed list compiles it; hb-stage0 dies at its first call with the bare token and exit 70" type
   NL ;

: STALE-SEEDED ( n -- ) {: i:n :}
   BAD-N @ 1+ BAD-N !
   s" BOOTSTRAP-MIRROR tools/bootstrap-mirror-lint.f: exemption `" type
   i EXEMPT-PATH$ type
   s" ` is stale - the seed source now compiles it (" type
   i EXEMPT-WHY$ type
   s" ); drop the row" type
   NL ;

: STALE-UNPINNED ( n -- ) {: i:n :}
   BAD-N @ 1+ BAD-N !
   s" BOOTSTRAP-MIRROR tools/bootstrap-mirror-lint.f: exemption `" type
   i EXEMPT-PATH$ type
   s" ` is stale - tools/boot-pin.f no longer names it (" type
   i EXEMPT-WHY$ type
   s" ); drop the row" type
   NL ;

: ARRAY-DEAD ( -- )
   BAD-N @ 1+ BAD-N !
   s" BOOTSTRAP-MIRROR tools/bootstrap.sh: emit_src no longer expands SRC_COMMON, so no array entry reaches a stage source" type
   NL ;

: PIN-LIST-GONE ( -- )
   BAD-N @ 1+ BAD-N !
   s" BOOTSTRAP-MIRROR tools/boot-pin.f: BP-EACH is not there, so the boot-prefix list cannot be read" type
   NL ;

\ ---- the check -----------------------------------------------------------------
: CHECK-ROW ( ptr u8 n -- )
   2dup EXEMPT? if 2drop exit then
   2dup SEED-HAS? if 2drop exit then
   MISSING-ROW ;

: EXEMPT-CK ( -- )
   EXEMPT-N 0 ?do
      i EXEMPT-PATH$ SEED-HAS? if i STALE-SEEDED then
      i EXEMPT-PATH$ PIN-HAS? 0= if i STALE-UNPINNED then
   loop ;

: SEED-LIST-CK ( ptr u8 n ptr u8 n -- )    \ bootstrap.sh path, boot-pin.f path
   {: sa:ptr su:n pa:ptr pu:n :}
   sa su SEED-SOURCES
   pa pu LEX-FILE
   ARRAY-USED @ 0= if ARRAY-DEAD then
   PIN-BODY-START 0 < if PIN-LIST-GONE exit then
   [: CHECK-ROW ;] PIN-EACH
   EXEMPT-CK ;

: RUN ( -- )
   RESET
   ROOTS-N 0 ?do
      i ROOT-AT [: WALK-FILE ;] WALK-FILES
   loop
   CORPUS-DRIFT-CK
   s" tools/bootstrap.sh" s" tools/boot-pin.f" SEED-LIST-CK
   FINISH ;

;package
