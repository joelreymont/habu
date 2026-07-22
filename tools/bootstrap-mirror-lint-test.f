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
private

256 constant PATH-CAP
create ROOT-BUF PATH-CAP allot
create FIX-BUF PATH-CAP allot
variable ROOT-U
variable FIX-U

: ROOT ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: FIX ( -- ptr u8 n )
   FIX-BUF FIX-U @ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-bml" TMPDIR-MKDIR {: a:ptr u:n :}
   u PATH-CAP > if E-FS-PATH throw then
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT CLEANUP-TREE+
   ROOT s" overlay.f" FIX-BUF JOIN-PATH FIX-U ! ;

\ 1. the REAL recovery corpus is clean: src/ carries no ADT declaration, so
\    the tripwire walk reports zero findings (the dot's contract holds).
: TEST-SRC-CLEAN ( -- )
   BOOTSTRAP-MIRROR-LINT-RESET
   s" src" [: BML-WALK-FILE ;] WALK-FILES
   BML-FILES @ 0 > TTRUE
   BML-BAD @ 0 T= ;

\ 2. red fixture: a planted declaration under a src-like label fires once per
\    keyword, with live tokens only (definition names and escaped references
\    stay silent - the grammar's own `: SUMTYPE` definers must not fire).
: DIRTY$ ( -- ptr u8 n )
   SB-RESET
   s" \ overlay fixture" SB-APPEND $0A SB-APPEND-C
   s" SUMTYPE zzbml 0 VARIANT one n ;VARIANT ;SUMTYPE" SB-APPEND $0A SB-APPEND-C
   s" : ENUM ( -- ) ;" SB-APPEND $0A SB-APPEND-C
   s" ' SUMTYPE drop" SB-APPEND $0A SB-APPEND-C
   s" postpone product" SB-APPEND $0A SB-APPEND-C
   s" PRIM: SUMTYPE PRIM;" SB-APPEND $0A SB-APPEND-C
   SB$ ;

: TEST-OVERLAY-FIRES ( -- )
   FIX DIRTY$ WRITE-ALL
   BOOTSTRAP-MIRROR-LINT-RESET
   FIX s" src/zz-overlay.f" BOOTSTRAP-MIRROR-LINT-FILE-AS
   BML-BAD @ 1 T=
   [: BOOTSTRAP-MIRROR-LINT-FINISH ;] catch 1 T= ;

\ 3. a keyword-free src-like file reports nothing and FINISH stays quiet.
: CLEAN$ ( -- ptr u8 n )
   SB-RESET
   s" : BMT-OK ( n -- n ) 1 + ;" SB-APPEND $0A SB-APPEND-C
   SB$ ;

: TEST-OVERLAY-CLEAN ( -- )
   FIX CLEAN$ WRITE-ALL
   BOOTSTRAP-MIRROR-LINT-RESET
   FIX s" src/zz-overlay.f" BOOTSTRAP-MIRROR-LINT-FILE-AS
   BML-BAD @ 0 T=
   [: BOOTSTRAP-MIRROR-LINT-FINISH ;] catch 0 T= ;

: RUN ( -- )
   T-RESET
   PREPARE
   TEST-SRC-CLEAN
   TEST-OVERLAY-FIRES
   TEST-OVERLAY-CLEAN
   CLEANUP-RUN
   T-REPORT ;

RUN
;package
