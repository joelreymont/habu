\ bootstrap-mirror-lint-test.f - focused coverage for the recovery-corpus tripwire.
\ Run: bin/hb --load tools/bootstrap-mirror-lint-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/bootstrap-mirror-lint.f

package BOOTSTRAP-MIRROR-LINT
using LINT-SPLIT
private

256 constant PATH-CAP
create ROOT-BUF PATH-CAP allot
create DIRTY-BUF PATH-CAP allot
create IGNORED-BUF PATH-CAP allot
variable ROOT-U
variable DIRTY-U
variable IGNORED-U

: ROOT ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: DIRTY ( -- ptr u8 n )
   DIRTY-BUF DIRTY-U @ ;

: IGNORED ( -- ptr u8 n )
   IGNORED-BUF IGNORED-U @ ;

\ ---- fixtures for the boot-prefix/seed check -----------------------------------
\ The check reads two real files, so each fixture is a COPY of the real file with
\ one edit: the decoys are shapes a text matcher counts and a field-role parse
\ must not, and the removal is the defect this dot fixed.
$8000 constant FIX-CAP
create FIX-IN FIX-CAP allot                \ the real script, read once
create FIX-OUT FIX-CAP allot               \ the fixture being written
variable FIX-U
variable FIX-MODE
variable REAL-N                            \ rows the real script yields
create SCRIPT-FIX-BUF PATH-CAP allot
create PIN-FIX-BUF PATH-CAP allot
variable SCRIPT-FIX-U
variable PIN-FIX-U

0 constant MODE-DECOY
1 constant MODE-COND
2 constant MODE-DROP

: SCRIPT-FIX ( -- ptr u8 n )
   SCRIPT-FIX-BUF SCRIPT-FIX-U @ ;

: PIN-FIX ( -- ptr u8 n )
   PIN-FIX-BUF PIN-FIX-U @ ;

: OUT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   FIX-U @ u + FIX-CAP > if E-STR-CAPACITY throw then
   a FIX-OUT FIX-U @ + u BYTE-COPY
   FIX-U @ u + FIX-U ! ;

: OUT$ ( -- ptr u8 n )
   FIX-OUT FIX-U @ ;

: OUT-C ( n -- ) {: c:n :}
   FIX-U @ 1 + FIX-CAP > if E-STR-CAPACITY throw then
   c FIX-OUT FIX-U @ + c!
   FIX-U @ 1 + FIX-U ! ;

: LINE+ ( ptr u8 n -- )
   OUT+ LF OUT-C ;

\ Four paths a substring search finds and no field role admits: a comment, a
\ string argument, another redirect target, and a cat with no redirect at all.
: DECOY-LINES ( -- )
   S\"   # cat src/core/zzbml-decoy.f >> \"$out\"" LINE+
   S\"   printf 'cat src/core/zzbml-decoy.f >> \"$out\"\\n' >> \"$out\"" LINE+
   S\"   cat src/core/zzbml-decoy.f >> \"$other\"" LINE+
   s"   cat src/core/zzbml-decoy.f" LINE+ ;

\ A row with every field in the right role, inside a driver conditional: it
\ reaches at most one emission, so it is not in the seed.
: COND-LINES ( -- )
   S\"   if [[ \"$driver\" == \"src/habu/snap.f\" ]]; then" LINE+
   S\"     cat src/core/zzbml-cond.f >> \"$out\"" LINE+
   s"   fi" LINE+ ;

\ The same row outside emit_src entirely, where nothing ever runs it.
: OUTSIDE-LINE ( -- )
   S\" cat src/core/zzbml-outside.f >> \"$out\"" LINE+ ;

: DROP-ROW? ( ptr u8 n -- bool )
   FIX-MODE @ MODE-DROP <> if 2drop LINT-FALSE exit then
   s"   src/core/bytes.f" LINT-STR= ;

: FIX-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u DROP-ROW? if exit then
   a u LINE+
   a u s" SRC_COMMON=(" LINT-STR= FIX-MODE @ MODE-DECOY = and if
      s"   # src/core/zzbml-array.f" LINE+
   then
   a u s" emit_src() {" LINT-STR= 0= if exit then
   FIX-MODE @ MODE-DECOY = if DECOY-LINES exit then
   FIX-MODE @ MODE-COND = if COND-LINES then ;

: BUILD-SCRIPT ( n -- )                    \ mode -> the fixture script on disk
   FIX-MODE !
   0 FIX-U !
   s" tools/bootstrap.sh" FIX-IN FIX-CAP READ-FILE SPLIT-LINES
   SN# @ 0 ?do i S@ FIX-LINE loop
   FIX-MODE @ MODE-DECOY = if OUTSIDE-LINE then
   SCRIPT-FIX OUT$ WRITE-ALL ;

\ boot-pin's copy carries the same two evasions: a row spelled inside a comment
\ within BP-EACH, and a live row string in a word outside it.
: PIN-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LINE+
   a u S\"    s\" src/core/roles.f\" q execute" LINT-STR= 0= if exit then
   S\"    \\ s\" src/core/zzbml-pin.f\" q execute" LINE+ ;

: BUILD-PIN ( -- )
   0 FIX-U !
   s" tools/boot-pin.f" FIX-IN FIX-CAP READ-FILE SPLIT-LINES
   SN# @ 0 ?do i S@ PIN-LINE loop
   S\" : BML-PIN-DECOY ( -- ptr u8 n ) s\" src/core/zzbml-pin.f\" ;" LINE+
   PIN-FIX OUT$ WRITE-ALL ;

\ 4. the real pair agrees: every boot-pin src/ row is compiled by the seed, the
\    one exemption is not, and a driver-conditional row is not counted.
: TEST-SEED-REAL ( -- )
   RESET
   s" tools/bootstrap.sh" s" tools/boot-pin.f" SEED-LIST-CK
   BAD-N @ 0 T=
   SEED-N @ REAL-N !
   SEED-N @ 0 > TTRUE
   s" src/core/bytes.f" SEED-HAS? TTRUE
   s" src/core/include.f" SEED-HAS? TTRUE
   s" src/core/internal-mark.f" SEED-HAS? TFALSE
   s" src/habu/aot-file.f" SEED-HAS? TFALSE
   s" src/core/internal-mark.f" PIN-HAS? TTRUE ;

\ 5. neither list gains a row from a comment, a string, a foreign redirect, or a
\    line outside the two lists - on either side of the check.
: TEST-DECOY ( -- )
   MODE-DECOY BUILD-SCRIPT
   BUILD-PIN
   RESET
   SCRIPT-FIX PIN-FIX SEED-LIST-CK
   BAD-N @ 0 T=
   SEED-N @ REAL-N @ T=
   s" src/core/zzbml-decoy.f" SEED-HAS? TFALSE
   s" src/core/zzbml-array.f" SEED-HAS? TFALSE
   s" src/core/zzbml-outside.f" SEED-HAS? TFALSE
   s" src/core/zzbml-pin.f" PIN-HAS? TFALSE ;

\ 6. a perfectly shaped cat row inside a conditional is not in the seed.
: TEST-CONDITIONAL ( -- )
   MODE-COND BUILD-SCRIPT
   RESET
   SCRIPT-FIX s" tools/boot-pin.f" SEED-LIST-CK
   BAD-N @ 0 T=
   SEED-N @ REAL-N @ T=
   s" src/core/zzbml-cond.f" SEED-HAS? TFALSE ;

\ 7. remove one real row and the check reds naming it - the defect this dot fixed
\    (hb-stage0 died `BYTE-COPY` with src/core/bytes.f absent).
: TEST-MISSING-ROW ( -- )
   MODE-DROP BUILD-SCRIPT
   RESET
   SCRIPT-FIX s" tools/boot-pin.f" SEED-LIST-CK
   s" src/core/bytes.f" SEED-HAS? TFALSE
   SEED-N @ REAL-N @ 1 - T=
   BAD-N @ 1 T=
   [: FINISH ;] catch 1 T= ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-bml" TMPDIR-MKDIR {: a:ptr u:n :}
   u PATH-CAP > if E-FS-PATH throw then
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT CLEANUP-TREE+
   ROOT s" dirty.f" DIRTY-BUF JOIN-PATH DIRTY-U !
   ROOT s" ignored.f" IGNORED-BUF JOIN-PATH IGNORED-U !
   ROOT s" bootstrap.sh" SCRIPT-FIX-BUF JOIN-PATH SCRIPT-FIX-U !
   ROOT s" boot-pin.f" PIN-FIX-BUF JOIN-PATH PIN-FIX-U ! ;

\ 1. the REAL recovery corpus is clean: src/ carries no ADT declaration, so
\    the tripwire walk reports zero findings (the dot's contract holds).
: TEST-SRC-CLEAN ( -- )
   RUN
   FILE-N @ 0 > TTRUE
   BAD-N @ 0 T= ;

\ 2. every live declaration keyword is found case-insensitively.
: DIRTY$ ( -- ptr u8 n )
   SB-RESET
   s" SuMtYpE zzbml-sum 0 VARIANT one n ;VARIANT ;SUMTYPE" SB-APPEND $0A SB-APPEND-C
   s" eNuM zzbml-enum one ;ENUM" SB-APPEND $0A SB-APPEND-C
   s" PrOdUcT zzbml-product 0 FIELD item n ;PRODUCT" SB-APPEND $0A SB-APPEND-C
   s" NeWtYpE zzbml-family 0" SB-APPEND $0A SB-APPEND-C
   SB$ ;

: TEST-LIVE-KEYWORDS ( -- )
   DIRTY DIRTY$ WRITE-ALL
   RESET
   DIRTY s" src/dirty.f" FILE-AS
   BAD-N @ 4 T=
   [: FINISH ;] catch 1 T= ;

\ 3. comments, strings, definition names, and escaped references stay silent.
: IGNORED$ ( -- ptr u8 n )
   SB-RESET
   s" \ SUMTYPE ENUM PRODUCT NEWTYPE" SB-APPEND $0A SB-APPEND-C
   S\" : BMT-TEXT ( -- ) s\" SUMTYPE ENUM PRODUCT NEWTYPE\" 2drop ;" SB-APPEND $0A SB-APPEND-C
   s" : SUMTYPE ( -- ) ;" SB-APPEND $0A SB-APPEND-C
   s" : ENUM ( -- ) ;" SB-APPEND $0A SB-APPEND-C
   s" : PRODUCT ( -- ) ;" SB-APPEND $0A SB-APPEND-C
   s" : NEWTYPE ( -- ) ;" SB-APPEND $0A SB-APPEND-C
   s" ' SUMTYPE ' ENUM ' PRODUCT ' NEWTYPE" SB-APPEND $0A SB-APPEND-C
   s" ['] SUMTYPE ['] ENUM ['] PRODUCT ['] NEWTYPE" SB-APPEND $0A SB-APPEND-C
   s" postpone SUMTYPE postpone ENUM postpone PRODUCT postpone NEWTYPE" SB-APPEND $0A SB-APPEND-C
   s" char SUMTYPE char ENUM char PRODUCT char NEWTYPE" SB-APPEND $0A SB-APPEND-C
   s" [char] SUMTYPE [char] ENUM [char] PRODUCT [char] NEWTYPE" SB-APPEND $0A SB-APPEND-C
   SB$ ;

: TEST-IGNORED-KEYWORDS ( -- )
   IGNORED IGNORED$ WRITE-ALL
   RESET
   IGNORED s" src/ignored.f" FILE-AS
   BAD-N @ 0 T=
   [: FINISH ;] catch 0 T= ;


: TESTS ( -- )
   T-RESET
   PREPARE
   TEST-SRC-CLEAN
   TEST-LIVE-KEYWORDS
   TEST-IGNORED-KEYWORDS
   TEST-SEED-REAL
   TEST-DECOY
   TEST-CONDITIONAL
   TEST-MISSING-ROW
   CLEANUP-RUN
   T-REPORT ;

TESTS
;using
;package
