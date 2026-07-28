\ suite-coverage-lint-test.f - checked fixtures for the suite-coverage lint.
\ Load after lib/test.f and tools/suite-coverage-lint-core.f.
\
\ Fixtures are built in scratch buffers with real newlines: cases members are the
\ first token of a line (beginning-of-line), so single-line s" literals cannot
\ carry them. Scheduled/ptx tokens do not need the s" quote - SC-STRIP-Q tolerates
\ a bare path - so `<path> GSI-INCLUDE` stands in for `s" <path>" GSI-INCLUDE`.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/suite-coverage-lint-core.f
require test/checker-assert.f

package SUITE-COVERAGE-LINT

4096 constant SCT-CAP
create SCT-CASE-BUF SCT-CAP allot     \ cases fixture (holds live member pointers)
create SCT-GX-BUF   SCT-CAP allot     \ scheduled / ptx-inproc fixture (scanned, not held)
variable SCT-CASE-U
variable SCT-GX-U

: SCT-CASE-RESET ( -- ) SCT-CASE-U STR:BUF-RESET ;
: SCT-CASE+ ( ptr u8 n -- ) STR:LENGTH SCT-CASE-BUF SCT-CAP STR:LENGTH SCT-CASE-U STR:BUF-APPEND ;
: SCT-CASE-NL ( -- ) 10 SCT-CASE-BUF SCT-CAP STR:LENGTH SCT-CASE-U STR:BUF-APPEND-C ;
: SCT-CASE-LINE ( ptr u8 n -- ) SCT-CASE+ SCT-CASE-NL ;
: SCT-CASE$ ( -- ptr u8 n ) SCT-CASE-BUF SCT-CASE-U @ LEN>N ;

: SCT-GX-RESET ( -- ) SCT-GX-U STR:BUF-RESET ;
: SCT-GX+ ( ptr u8 n -- ) STR:LENGTH SCT-GX-BUF SCT-CAP STR:LENGTH SCT-GX-U STR:BUF-APPEND ;
: SCT-GX-NL ( -- ) 10 SCT-GX-BUF SCT-CAP STR:LENGTH SCT-GX-U STR:BUF-APPEND-C ;
: SCT-GX-LINE ( ptr u8 n -- ) SCT-GX+ SCT-GX-NL ;
: SCT-GX$ ( -- ptr u8 n ) SCT-GX-BUF SCT-GX-U @ LEN>N ;

: SCT-QUIET ( -- ) 0 SC-REPORT? ! ;

\ ---- live tree is green -----------------------------------------------------
: SCT-LIVE-GREEN ( -- )
   \ the real gate files must classify every suite (0 findings, exit 0)
   [: RUN ;] catch 0 T= ;

: SCT-RESOLUTION ( -- )
   s" SCT-PUB ( -- ) SUITE-COVERAGE-LINT:RUN" CHECK-QUIET-CANDIDATE! -1 T=
   s" SCT-PRIV ( -- ) SUITE-COVERAGE-LINT:SC-RESET" CHECK-QUIET-CANDIDATE! 1 T=
   s" SCT-LEGACY ( -- ) SUITE-COVERAGE-LINT" CHECK-QUIET-CANDIDATE! 1 T= ;

\ ---- parse: cases members are BOL .f tokens; fake `-- file.f` args excluded ---
: SCT-PARSE-MEMBERS ( -- )
   SCT-QUIET SC-RESET
   SCT-CASE-RESET
   s" TEST:SUITE argv-demo" SCT-CASE-LINE
   s" lib/argv-test.f -- --json -o OUT -- file.f --literal" SCT-CASE-LINE
   s" TEST:;SUITE" SCT-CASE-LINE
   SCT-CASE$ SC-CASES-SCAN$
   \ one suite header, exactly one member (the mid-line fake file.f is not BOL)
   SC-SUITE# @ 1 T=
   SC-CASE# @ 1 T= ;

\ ---- bare `using TEST` manifest form is parsed like the qualified form --------
: SCT-PARSE-MEMBERS-BARE ( -- )
   SCT-QUIET SC-RESET
   SCT-CASE-RESET
   s" using TEST" SCT-CASE-LINE
   s" SUITE argv-demo" SCT-CASE-LINE
   s" lib/argv-test.f -- --json -o OUT -- file.f --literal" SCT-CASE-LINE
   s" ;SUITE" SCT-CASE-LINE
   s" ;using" SCT-CASE-LINE
   \ after ;using the bare SUITE is out of TEST scope: NOT a header, member ignored
   s" SUITE not-a-header" SCT-CASE-LINE
   s" leaked-test.f" SCT-CASE-LINE
   SCT-CASE$ SC-CASES-SCAN$
   \ exactly one in-scope suite, one member; the mid-line fake file.f and the
   \ post-;using SUITE/member are both excluded
   SC-SUITE# @ 1 T=
   SC-CASE# @ 1 T= ;

\ ---- bare maki form (SUITE <file> under using TEST) captures master entries ----
: SCT-MAKI-BARE ( -- )
   SCT-QUIET SC-RESET
   0 SC-MK-MODE !
   SCT-CASE-RESET
   s" using TEST" SCT-CASE-LINE
   s" GROUP SEQ demo" SCT-CASE-LINE
   s" SUITE maki/one-test.f" SCT-CASE-LINE
   s" ;SUITE" SCT-CASE-LINE
   s" SUITE maki/two-test.f" SCT-CASE-LINE
   s" ;SUITE" SCT-CASE-LINE
   s" ;GROUP" SCT-CASE-LINE
   s" ;using" SCT-CASE-LINE
   SCT-CASE$ SC-MK-SCAN$
   \ two master entries captured from the bare `SUITE <file>` headers
   SC-MK# @ 2 T= ;

\ ---- (a) orphan: a member in no scheduled group / table is a finding ---------
: SCT-ORPHAN ( -- )
   SCT-QUIET SC-RESET
   SCT-GX-RESET
   s" foo-test.f GSI-INCLUDE" SCT-GX-LINE
   SCT-GX$ SC-SCHED-SCAN$
   SCT-CASE-RESET
   s" TEST:SUITE demo" SCT-CASE-LINE
   s" foo-test.f" SCT-CASE-LINE
   s" orphan-xyz-test.f" SCT-CASE-LINE
   s" TEST:;SUITE" SCT-CASE-LINE
   SCT-CASE$ SC-CASES-SCAN$
   SC-CHECK-ORPHANS
   \ foo-test.f scheduled -> ok; orphan-xyz-test.f -> one SUITE-ORPHAN
   SC-FIND @ 1 T= ;

: SCT-CANDIDATE-SCHEDULE ( -- )
   SCT-QUIET SC-RESET
   SCT-GX-RESET
   s" candidate-test.f GE-SRC-FILE+" SCT-GX-LINE
   SCT-GX$ SC-SCHED-SCAN$
   s" candidate-test.f" SC-SCHEDULED? TTRUE
   SCT-GX-RESET
   s" candidate-negative.f construct case-kind negative 70" SCT-GX-LINE
   s" ARMED diagnostic RUN-CASE" SCT-GX-LINE
   SCT-GX$ SC-CAND-SCAN$
   s" candidate-negative.f" SC-SCHEDULED? TTRUE ;

\ ---- (b) ptx: spawned unit absent from the inprocess list -> PTX-TOOL-MISSING -
: SCT-PTX-MISSING ( -- )
   SCT-QUIET SC-RESET
   SCT-CASE-RESET
   s" TEST:SUITE ptx-toolchain" SCT-CASE-LINE
   s" a-test.f" SCT-CASE-LINE
   s" b-test.f" SCT-CASE-LINE
   s" TEST:;SUITE" SCT-CASE-LINE
   SCT-CASE$ SC-CASES-SCAN$
   SCT-GX-RESET
   s" : GSI-LINT-LIBS-PTX-TOOL" SCT-GX-LINE
   s" a-test.f GSI-INCLUDE ;" SCT-GX-LINE
   SCT-GX$ SC-PTX-INPROC-SCAN$
   SC-CHECK-PTX
   \ b-test.f spawned, not inprocess, not spawn-only -> one PTX-TOOL-MISSING
   SC-FIND @ 1 T= ;

\ ---- (b) ptx: inprocess entry that is not a spawned unit -> PTX-TOOL-EXTRA ----
: SCT-PTX-EXTRA ( -- )
   SCT-QUIET SC-RESET
   SCT-CASE-RESET
   s" TEST:SUITE ptx-toolchain" SCT-CASE-LINE
   s" a-test.f" SCT-CASE-LINE
   s" TEST:;SUITE" SCT-CASE-LINE
   SCT-CASE$ SC-CASES-SCAN$
   SCT-GX-RESET
   s" : GSI-LINT-LIBS-PTX-TOOL" SCT-GX-LINE
   s" a-test.f GSI-INCLUDE" SCT-GX-LINE
   s" c-test.f GSI-INCLUDE ;" SCT-GX-LINE
   SCT-GX$ SC-PTX-INPROC-SCAN$
   SC-CHECK-PTX
   \ c-test.f inprocess but not a spawned ptx unit -> one PTX-TOOL-EXTRA
   SC-FIND @ 1 T= ;

\ ---- (b) a documented spawn-only file must NOT run inprocess ------------------
: SCT-PTX-SPAWN-ONLY-INPROC ( -- )
   SCT-QUIET SC-RESET
   SCT-CASE-RESET
   s" TEST:SUITE ptx-toolchain" SCT-CASE-LINE
   s" a-test.f" SCT-CASE-LINE
   s" tools/ptx/gemm-bench.f" SCT-CASE-LINE
   s" TEST:;SUITE" SCT-CASE-LINE
   SCT-CASE$ SC-CASES-SCAN$
   SCT-GX-RESET
   s" : GSI-LINT-LIBS-PTX-TOOL" SCT-GX-LINE
   s" a-test.f GSI-INCLUDE" SCT-GX-LINE
   s" tools/ptx/gemm-bench.f GSI-INCLUDE ;" SCT-GX-LINE
   SCT-GX$ SC-PTX-INPROC-SCAN$
   SC-CHECK-PTX
   \ gemm-bench is spawn-only; running it inprocess -> one PTX-TOOL-EXTRA
   SC-FIND @ 1 T= ;

\ ---- (b) exact equality (spawned minus spawn-only == inprocess) is clean ------
: SCT-PTX-CLEAN ( -- )
   SCT-QUIET SC-RESET
   SCT-CASE-RESET
   s" TEST:SUITE ptx-toolchain" SCT-CASE-LINE
   s" a-test.f" SCT-CASE-LINE
   s" tools/ptx/gemm-bench.f" SCT-CASE-LINE
   s" TEST:;SUITE" SCT-CASE-LINE
   SCT-CASE$ SC-CASES-SCAN$
   SCT-GX-RESET
   s" : GSI-LINT-LIBS-PTX-TOOL" SCT-GX-LINE
   s" a-test.f GSI-INCLUDE ;" SCT-GX-LINE
   SCT-GX$ SC-PTX-INPROC-SCAN$
   SC-CHECK-PTX
   \ inprocess == {a-test.f} == spawned {a-test.f, gemm-bench} minus spawn-only
   SC-FIND @ 0 T= ;

\ ---- (c) table hygiene: a manual entry that is actually scheduled is stale ----
: SCT-MANUAL-STALE ( -- )
   SCT-QUIET SC-RESET
   s" lib/hashmap-test.f" SC-SCHED+
   s" lib/hashmap-test.f" SC-MANUAL-STALE-CHECK
   \ documented manual but now scheduled -> one MANUAL-STALE
   SC-FIND @ 1 T= ;

: SCT-SPAWN-STALE ( -- )
   SCT-QUIET SC-RESET
   s" tools/ptx/gemm-bench.f" SC-SCHED+
   s" tools/ptx/gemm-bench.f" SC-SPAWN-STALE-CHECK
   \ documented spawn-only but now scheduled -> one SPAWN-ONLY-STALE
   SC-FIND @ 1 T= ;

\ ---- (d) a tools/lint/*-test.f absent from every TEST:SUITE is a finding ------
: SCT-LINT-UNREGISTERED ( -- )
   SCT-QUIET SC-RESET
   SCT-CASE-RESET
   s" TEST:SUITE demo" SCT-CASE-LINE
   s" tools/lint/registered-test.f" SCT-CASE-LINE
   s" TEST:;SUITE" SCT-CASE-LINE
   SCT-CASE$ SC-CASES-SCAN$
   \ a non-test source under tools/lint is not a lint test -> ignored
   s" tools/lint/registered.f" SC-LINT-REG-VISIT
   SC-FIND @ 0 T=
   \ a registered lint test is a cases member -> ok
   s" tools/lint/registered-test.f" SC-LINT-REG-VISIT
   SC-FIND @ 0 T=
   \ an unregistered lint test file -> one LINT-UNREGISTERED
   s" tools/lint/unregistered-test.f" SC-LINT-REG-VISIT
   SC-FIND @ 1 T= ;

\ ---- (e) test-tree routing ---------------------------------------------------
\ The rows normally come from a walk of test/; here they are pushed directly so
\ the classifier is exercised without touching the live tree.
: SCT-ROUTE-ROWS ( -- )
   SCT-QUIET SC-RESET
   s" test/alpha.f" SC-TF+          \ row 0: self-running, loaded by nobody
   s" test/beta.f" SC-TF+           \ row 1: self-running, loaded by row 3
   s" test/helper.f" SC-TF+         \ row 2: no verdict word, so not a test
   s" test/parent.f" SC-TF+ ;       \ row 3: self-running and a cases member

\ Scan one row's source with that row selected, exactly as the walk does: the
\ same manifest-skip predicate decides whether the row is read as a loader.
: SCT-SCAN-ROW ( n ptr u8 n -- ) {: k:n a:ptr u:n :}
   k SC-TF-SELF !
   k SC-TF-LOADER-ROW? 0= if exit then
   a u SC-TF-SCAN$ ;

\ Fixtures for check (e) carry real double quotes, which a single-line s" literal
\ cannot, so they are assembled token by token in the scratch buffer.
: SCT-GX-SP ( -- ) 32 SCT-GX-BUF SCT-CAP STR:LENGTH SCT-GX-U STR:BUF-APPEND-C ;
: SCT-GX-Q ( -- ) SC-DQUOTE SCT-GX-BUF SCT-CAP STR:LENGTH SCT-GX-U STR:BUF-APPEND-C ;
: SCT-GX-TOK+ ( ptr u8 n -- ) SCT-GX-SP SCT-GX+ ;
: SCT-GX-OPENER ( -- ) s" s" SCT-GX-TOK+ SCT-GX-Q ;      \ the token s"
: SCT-GX-CLOSED ( ptr u8 n -- ) SCT-GX-TOK+ SCT-GX-Q ;   \ a token that closes the literal

: SCT-TEST-UNROUTED ( -- )
   SCT-ROUTE-ROWS
   SCT-CASE-RESET
   s" TEST:SUITE demo" SCT-CASE-LINE
   s" test/parent.f" SCT-CASE-LINE
   s" TEST:;SUITE" SCT-CASE-LINE
   SCT-CASE$ SC-CASES-SCAN$
   \ alpha runs a verdict and names nothing
   0 s" : RUN T-REPORT ;" SCT-SCAN-ROW
   \ beta likewise
   1 s" : RUN T-REPORT ;" SCT-SCAN-ROW
   \ helper defines words only: no verdict word, so it is not a candidate
   2 s" : HELP 1 ;" SCT-SCAN-ROW
   \ parent runs a verdict, requires beta, and spawns nothing else
   3 s" require test/beta.f : RUN T-REPORT ;" SCT-SCAN-ROW
   0 SC-TF-RUNS? TTRUE
   2 SC-TF-RUNS? TFALSE
   1 SC-TF-NAMED? TTRUE
   0 SC-TF-NAMED? TFALSE
   SC-CHECK-TEST-ROUTE
   \ beta is loaded by parent, parent is a cases member, helper is not a test:
   \ only alpha is unrouted
   SC-FIND @ 1 T= ;

\ A path inside a longer string literal is prose, not a load: the generated Rocq
\ header naming test/compiler/ir-id-proof.f is why this distinction exists.
: SCT-ROUTE-PROSE ( -- )
   SCT-ROUTE-ROWS
   0 s" : RUN T-REPORT ;" SCT-SCAN-ROW
   \ parent mentions alpha in the MIDDLE of a literal: ` : NOTE s" see <path> rest" TYPE ;`
   SCT-GX-RESET
   s" : NOTE" SCT-GX-TOK+
   SCT-GX-OPENER
   s" see" SCT-GX-TOK+
   s" test/alpha.f" SCT-GX-TOK+
   s" rest" SCT-GX-CLOSED
   s" TYPE ; : RUN T-REPORT ;" SCT-GX-TOK+
   3 SCT-GX$ SCT-SCAN-ROW
   0 SC-TF-NAMED? TFALSE
   SC-CHECK-TEST-ROUTE
   \ alpha AND parent are both unrouted here: prose exempts neither
   SC-FIND @ 2 T= ;

\ A whole one-word string literal IS a load: it is the argument every child
\ process spawn builds.
: SCT-ROUTE-SPAWN ( -- )
   SCT-ROUTE-ROWS
   0 s" : RUN T-REPORT ;" SCT-SCAN-ROW
   \ parent spawns alpha: ` : CHILD s" <path>" RUN-FILE ;`
   SCT-GX-RESET
   s" : CHILD" SCT-GX-TOK+
   SCT-GX-OPENER
   s" test/alpha.f" SCT-GX-CLOSED
   s" RUN-FILE ; : RUN T-REPORT ;" SCT-GX-TOK+
   3 SCT-GX$ SCT-SCAN-ROW
   0 SC-TF-NAMED? TTRUE ;

\ A file naming itself proves nothing.
: SCT-ROUTE-SELF ( -- )
   SCT-ROUTE-ROWS
   0 s" require test/alpha.f : RUN T-REPORT ;" SCT-SCAN-ROW
   0 SC-TF-NAMED? TFALSE ;

\ The gate manifests are read as scheduling, never as "another file loads it",
\ so deleting a scheduling entry cannot be hidden by the manifest's own text.
: SCT-ROUTE-MANIFEST ( -- )
   SCT-ROUTE-ROWS
   s" test/gate-demo-cases.f" SC-TF+          \ row 4
   s" test/gate-demo-cases.f" SC-MF+
   \ the manifest schedules alpha: ` s" <path>" GSI-FORK-INCLUDE`
   SCT-GX-RESET
   SCT-GX-OPENER
   s" test/alpha.f" SCT-GX-CLOSED
   s" GSI-FORK-INCLUDE" SCT-GX-TOK+
   4 SCT-GX$ SCT-SCAN-ROW
   \ the manifest row is skipped by the walk, so alpha stays unnamed
   0 SC-TF-NAMED? TFALSE ;

\ (e) table hygiene: a documented host-gated row something already runs is stale.
: SCT-HOST-GATED-STALE ( -- )
   SCT-ROUTE-ROWS
   0 s" : RUN T-REPORT ;" SCT-SCAN-ROW
   s" test/alpha.f" SC-HOST-STALE-CHECK
   \ self-running and unrouted: a legitimate host-gated row, no finding
   SC-FIND @ 0 T=
   s" test/helper.f" SC-HOST-STALE-CHECK
   \ documented host-gated but runs no verdict word -> one HOST-GATED-STALE
   SC-FIND @ 1 T=
   s" test/not-a-file.f" SC-HOST-STALE-CHECK
   \ documented host-gated but not a file under test/ -> a second finding
   SC-FIND @ 2 T= ;

: SCT-MAIN ( -- )
   T-RESET
   SCT-LIVE-GREEN
   SCT-RESOLUTION
   SCT-PARSE-MEMBERS
   SCT-PARSE-MEMBERS-BARE
   SCT-MAKI-BARE
   SCT-ORPHAN
   SCT-CANDIDATE-SCHEDULE
   SCT-PTX-MISSING
   SCT-PTX-EXTRA
   SCT-PTX-SPAWN-ONLY-INPROC
   SCT-PTX-CLEAN
   SCT-MANUAL-STALE
   SCT-SPAWN-STALE
   SCT-LINT-UNREGISTERED
   SCT-TEST-UNROUTED
   SCT-ROUTE-PROSE
   SCT-ROUTE-SPAWN
   SCT-ROUTE-SELF
   SCT-ROUTE-MANIFEST
   SCT-HOST-GATED-STALE
   T-REPORT ;

SCT-MAIN

;package
