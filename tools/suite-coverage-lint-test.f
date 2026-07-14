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

4096 constant SCT-CAP
create SCT-CASE-BUF SCT-CAP allot     \ cases fixture (holds live member pointers)
create SCT-GX-BUF   SCT-CAP allot     \ scheduled / ptx-inproc fixture (scanned, not held)
variable SCT-CASE-U
variable SCT-GX-U

: SCT-CASE-RESET ( -- ) SCT-CASE-U BUF-RESET ;
: SCT-CASE+ ( ptr u8 n -- ) SCT-CASE-BUF SCT-CAP SCT-CASE-U BUF-APPEND ;
: SCT-CASE-NL ( -- ) 10 SCT-CASE-BUF SCT-CAP SCT-CASE-U BUF-APPEND-C ;
: SCT-CASE-LINE ( ptr u8 n -- ) SCT-CASE+ SCT-CASE-NL ;
: SCT-CASE$ ( -- ptr u8 n ) SCT-CASE-BUF SCT-CASE-U @ LEN>N ;

: SCT-GX-RESET ( -- ) SCT-GX-U BUF-RESET ;
: SCT-GX+ ( ptr u8 n -- ) SCT-GX-BUF SCT-CAP SCT-GX-U BUF-APPEND ;
: SCT-GX-NL ( -- ) 10 SCT-GX-BUF SCT-CAP SCT-GX-U BUF-APPEND-C ;
: SCT-GX-LINE ( ptr u8 n -- ) SCT-GX+ SCT-GX-NL ;
: SCT-GX$ ( -- ptr u8 n ) SCT-GX-BUF SCT-GX-U @ LEN>N ;

: SCT-QUIET ( -- ) 0 SC-REPORT? ! ;

\ ---- live tree is green -----------------------------------------------------
: SCT-LIVE-GREEN ( -- )
   \ the real gate files must classify every suite (0 findings, exit 0)
   [: SUITE-COVERAGE-LINT ;] catch 0 T= ;

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

: SCT-MAIN ( -- )
   T-RESET
   SCT-LIVE-GREEN
   SCT-PARSE-MEMBERS
   SCT-ORPHAN
   SCT-PTX-MISSING
   SCT-PTX-EXTRA
   SCT-PTX-SPAWN-ONLY-INPROC
   SCT-PTX-CLEAN
   SCT-MANUAL-STALE
   SCT-SPAWN-STALE
   T-REPORT ;

SCT-MAIN
