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

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-bml" TMPDIR-MKDIR {: a:ptr u:n :}
   u PATH-CAP > if E-FS-PATH throw then
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT CLEANUP-TREE+
   ROOT s" dirty.f" DIRTY-BUF JOIN-PATH DIRTY-U !
   ROOT s" ignored.f" IGNORED-BUF JOIN-PATH IGNORED-U ! ;

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
   CLEANUP-RUN
   T-REPORT ;

TESTS
;package
