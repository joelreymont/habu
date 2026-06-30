\ check-test.f - process-boundary smoke coverage for tools/check.f.
\ Run: bin/hb --load tools/date.f lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/source.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/json-writer.f tools/lint/source-lex.f
\ tools/diag-origin-core.f tools/json.f tools/json-only-core.f
\ tools/signature-lint-core.f tools/checked-boundary-lint-core.f
\ tools/reserved-name-lint-core.f
\ tools/trust-lint-core.f tools/check-all-errors-core.f tools/argv.f
\ tools/warm-run.f tools/check-core.f tools/check-test.f

require tools/date.f
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/source.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f
require tools/diag-origin-core.f
require tools/json.f
require tools/json-only-core.f
require tools/signature-lint-core.f
require tools/checked-boundary-lint-core.f
require tools/reserved-name-lint-core.f
require tools/trust-lint-core.f
require tools/check-all-errors-core.f
require tools/argv.f
require tools/warm-run.f
require tools/check-core.f

$4000 constant CKT-BUF-CAP
60000 constant CKT-TIMEOUT-MS

create CKT-ROOT FS-PATH-CAP allot
create CKT-BAD-PATH FS-PATH-CAP allot
create CKT-LIST-PATH FS-PATH-CAP allot

variable CKT-OUT-A
variable CKT-ERR-A
variable CKT-ROOT-U
variable CKT-BAD-U
variable CKT-LIST-U
variable CKT-USE-CHECK

: CKT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: CKT-ROOT$ ( -- ptr u8 n )
   CKT-ROOT CKT-ROOT-U @ ;

: CKT-BAD$ ( -- ptr u8 n )
   CKT-BAD-PATH CKT-BAD-U @ ;

: CKT-LIST$ ( -- ptr u8 n )
   CKT-LIST-PATH CKT-LIST-U @ ;

: CKT-OUT-A-FIELD ( -- ptr ptr u8 )
   CKT-OUT-A 0 ptr-field ;

: CKT-ERR-A-FIELD ( -- ptr ptr u8 )
   CKT-ERR-A 0 ptr-field ;

: CKT-OUT-A@ ( -- ptr u8 )
   CKT-OUT-A-FIELD @ ;

: CKT-ERR-A@ ( -- ptr u8 )
   CKT-ERR-A-FIELD @ ;

: CKT-OUT-A! ( ptr u8 -- )
   CKT-OUT-A-FIELD ! ;

: CKT-ERR-A! ( ptr u8 -- )
   CKT-ERR-A-FIELD ! ;

: CKT-ALLOC-BUF ( -- ptr u8 )
   CKT-BUF-CAP MEM-ALLOC-BYTES drop ;

: CKT-OUT ( -- ptr u8 )
   CKT-OUT-A @ 0= if CKT-ALLOC-BUF CKT-OUT-A! then
   CKT-OUT-A@ ;

: CKT-ERR ( -- ptr u8 )
   CKT-ERR-A @ 0= if CKT-ALLOC-BUF CKT-ERR-A! then
   CKT-ERR-A@ ;

: CKT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: CKT-EXE$ ( -- ptr u8 n )
   CKT-USE-CHECK @ if WR-CHECK$ exit then
   WR-TOOLS$ ;

: CKT-ARGV-BASE ( -- )
   PROC-ARGV-RESET
   0 CKT-USE-CHECK !
   s" tools/check-main.f" WR-CHECK-LOAD if -1 CKT-USE-CHECK ! exit then
   s" tools/check-core.f" s" tools/check-main.f" WR-TOOLS-LOAD2 if exit then
   s" --load"  >LEN PROC-ARGV+
   s" tools/check.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: CKT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: CKT-CAPTURE ( -- n n n )
   CKT-EXE$ >LEN CKT-OUT CKT-BUF-CAP >LEN CKT-ERR CKT-BUF-CAP >LEN
   CKT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   CKT-CAPTURE>N ;

: CKT-RUN-FILE-JSON ( -- n n n )
   CKT-ARGV-BASE
   s" --json-errors"  >LEN PROC-ARGV+
   CKT-BAD$  >LEN PROC-ARGV+
   CKT-CAPTURE ;

: CKT-RUN-ARGS ( -- n n n )
   CKT-ARGV-BASE
   s" --bad-flag"  >LEN PROC-ARGV+
   CKT-CAPTURE ;

: CKT-CORE-ACT ( -- )
   CKT-BAD$ CKT-BAD$ CHECK-ALL-ERRORS-FILE ;

: CKT-CORE-JSON ( ptr u8 n -- n n n ) {: src:ptr srcu:n :}
   CKT-BAD$ src srcu WRITE-ALL
   CKT-ERR CKT-BUF-CAP CKT-OUT CKT-BUF-CAP CHECK-ALL-ERRORS-BUFFERS!
   0 0= CHECK-ALL-ERRORS-JSON!
   [: CKT-CORE-ACT ;] catch {: rc:n :}
   0 CHECK-ALL-ERRORS-OUT$ nip rc ;

: CKT-DIRECT-START ( -- )
   CHK-RESET-CFG
   CKT-OUT CKT-BUF-CAP CKT-ERR CKT-BUF-CAP CHK-CAPTURE-BUFFERS! ;

: CKT-DIRECT-END ( n -- n n n ) {: rc:n :}
   CHK-CAPTURE-OUT$ nip
   CHK-CAPTURE-ERR$ nip
   rc ;

: CKT-DIRECT-STDIN ( ptr u8 n -- n n n ) {: src:ptr srcu:n :}
   CKT-DIRECT-START
   src srcu s" <stdin>" CHK-MATERIALIZE-BUF-AS
   CHK-DIRECT-RUN CKT-DIRECT-END ;

: CKT-DIRECT-SOURCE-LIST-PATH ( ptr u8 n -- n n n )
   CKT-DIRECT-START
   CHK-MATERIALIZE-LIST-PATH
   CHK-DIRECT-RUN CKT-DIRECT-END ;

: CKT-DIRECT-PREVERIFY-LIST-PATH ( ptr u8 n -- n n n )
   CKT-DIRECT-START
   CHK-MATERIALIZE-LIST-PATH
   [: CHK-RUN-PREVERIFY ;] catch CKT-DIRECT-END ;

: CKT-DIRECT-PREVERIFY-PATH ( ptr u8 n -- n n n )
   CKT-DIRECT-START
   CHK-ADD-POS
   CHK-MATERIALIZE
   [: CHK-RUN-PREVERIFY ;] catch CKT-DIRECT-END ;

: CKT-DIRECT-PATH ( ptr u8 n -- n n n )
   CKT-DIRECT-START
   CHK-ADD-POS
   CHK-MATERIALIZE
   CHK-DIRECT-RUN CKT-DIRECT-END ;

: CKT-GOOD$ ( -- ptr u8 n )
   s" : CKT-OK ( i64 -- i64 ) dup * ;" ;

: CKT-BAD$SRC ( -- ptr u8 n )
   s" : CKT-BAD-WORD ( i64 -- i64 ) dup ;" ;

: CKT-DUP$SRC ( -- ptr u8 n )
   SB-RESET
   s" : CKT-DUP ( i64 -- i64 ) 1 + ;" SB-APPEND
   $0a SB-APPEND-C
   s" : CKT-DUP ( i64 -- i64 ) 2 + ;" SB-APPEND
   SB$ ;

: CKT-RESERVED$ ( -- ptr u8 n )
   s" variable I" ;

: CKT-RUN-SOURCE-LIST-RESERVED-CORE ( -- n n n )
   CKT-LIST$ CKT-RESERVED$ WRITE-ALL
   RESERVED-NAME-LINT-RESET
   CKT-ERR CKT-BUF-CAP LINT-OUT-BUFFER!
   CKT-LIST$ s" <source-list>" RESERVED-NAME-LINT-FILE-AS
   [: RESERVED-NAME-LINT-FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip
   LINT-OUT-BUFFER-OFF
   0 swap rc ;

: CKT-DIE$ ( -- ptr u8 n )
   SB-RESET
   s" : CKT-BYE ( -- ) s" SB-APPEND
   $22 SB-APPEND-C
   s"  bye" SB-APPEND
   $22 SB-APPEND-C
   s"  5 die ;" SB-APPEND
   $0a SB-APPEND-C
   s" CKT-BYE" SB-APPEND
   SB$ ;

: CKT-UNTERM-SDQ$ ( -- ptr u8 n )
   SB-RESET
   s" : CKT-UNTERM-SDQ ( -- ptr u8 n ) s" SB-APPEND
   $22 SB-APPEND-C
   s"  nope ;" SB-APPEND
   SB$ ;

: CKT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-check-test" TMPDIR-MKDIR CKT-ROOT CKT-ROOT-U CKT-COPY!
   CKT-ROOT$ CLEANUP-TREE+
   CKT-ROOT$ s" bad.f" CKT-BAD-PATH JOIN-PATH CKT-BAD-U !
   CKT-ROOT$ s" local-test.f" CKT-LIST-PATH JOIN-PATH CKT-LIST-U !
   CKT-BAD$ CKT-BAD$SRC WRITE-ALL ;

: CKT-TEST-GOOD ( -- )
   [: CKT-GOOD$ VERIFY-SOURCE-BUF ;] catch 0 T= ;

: CKT-TEST-FILE-LABEL ( -- )
   CKT-RUN-FILE-JSON 70 T=
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru CKT-BAD$ CONTAINS? TTRUE ;

: CKT-TEST-USAGE ( -- )
   CKT-RUN-ARGS 64 T=
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru s" usage: tools/check.f" CONTAINS? TTRUE ;

: CKT-TEST-DIE ( -- )
   CKT-DIE$ CKT-DIRECT-STDIN 5 T=
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru s" bye" CONTAINS? TTRUE ;

: CKT-EXPECT-UNTERM-STRING ( ptr u8 n -- ) {: src:ptr srcu :}
   src srcu CKT-CORE-JSON 70 T=
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru s" E-" CONTAINS? TTRUE ;

: CKT-TEST-UNTERM-STRING ( -- )
   CKT-UNTERM-SDQ$ CKT-EXPECT-UNTERM-STRING ;

: CKT-TEST-DUP-ALL ( -- )
   CKT-DUP$SRC CKT-CORE-JSON $4E T=
   {: outu:n erru:n :}
   outu 0 T=
   CKT-ERR erru s" E-DUPLICATE-DEFINITION" CONTAINS? TTRUE
   CKT-ERR erru s" duplicate-definition" CONTAINS? TTRUE ;

: CKT-TEST-SOURCE-LIST-RESERVED ( -- )
   CKT-RUN-SOURCE-LIST-RESERVED-CORE 1 T=
   {: outu erru :}
   outu 0 T=
   CKT-ERR erru s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   CKT-ERR erru s" <source-list>" CONTAINS? TTRUE ;

: CKT-TEST-SOURCE-LIST-AUDITED-LIB ( -- )
   s" lib/test.f" CKT-DIRECT-PREVERIFY-LIST-PATH 0 T=
   {: outu erru :}
   outu 0 T=
   erru 0 T= ;

: CKT-TEST-REQUIRE-FACADE ( -- )
   s" lib/test/suite-test.f" CKT-DIRECT-PREVERIFY-PATH 0 T=
   {: outu:n erru:n :}
   erru 0 T=
   outu 0 T= ;

: CKT-MAIN ( -- )
   T-RESET
   CKT-PREPARE
   CKT-TEST-GOOD
   CKT-TEST-FILE-LABEL
   CKT-TEST-USAGE
   CKT-TEST-DIE
   CKT-TEST-UNTERM-STRING
   CKT-TEST-DUP-ALL
   CKT-TEST-SOURCE-LIST-RESERVED
   CKT-TEST-SOURCE-LIST-AUDITED-LIB
   CKT-TEST-REQUIRE-FACADE
   CLEANUP-RUN
   T-REPORT
   s" check-test: ok" type cr ;

CKT-MAIN
