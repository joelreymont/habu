\ gate-stdlib.f - Habu-specific adapter for the native lint/stdlib test phase.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, and lib/content-key.f.
\ The runner, the pool and the suite registry it drives are required below, so a
\ consumer that only wants the slice predicates - tools/lint/schedule-lint.f asks
\ them which labels a slice selects - does not have to reproduce a prefix.

require lib/adt/option.f                 \ option<n> STR>NUMBER? consumer (switchover wave A)
require lib/test.f
require lib/test/budget.f                \ T-BUDGET-MS: the per-suite wall scales with load
require lib/test/runner.f                \ GT-RESET / GT-RC@ / GT-OUT$
require test/gate-pool.f                 \ GT-POOL-START and the stats events beneath it

package STDLIB-GATE

public

\ The per-suite wall for one spawned child engine, on an idle box. It is a
\ NOMINAL figure, not the deadline: SUITE-TIMEOUT-MS stretches it by the measured
\ load factor the way every other per-suite budget in the tree does.
\
\ It used to be the deadline itself, and that made this the one budget in the
\ gate that could not tell a slow box from a hung child. compiler-insn-proof runs
\ 99543ms quiescent, so it had 20 percent of headroom against a fixed 120000; a
\ second full gate on the same host stretched it to 120145ms and the constant
\ called that a TIMEOUT-UNDER-LOAD. Raising the constant would have bought that
\ one suite room by deleting the hang detector for the ~180 suites that finish in
\ a second. Scaling keeps both: lib/test/budget.f clamps the factor to
\ [100% .. 300%], so a budget never shrinks below nominal and a genuinely hung
\ child still dies within 3x nominal.
120000 constant SUITE-TIMEOUT-NOMINAL-MS

\ The deadline a spawned suite is actually given: the nominal wall stretched by
\ HB_LOAD_PCT, which test/run-lib.f exports into every phase it starts and which
\ lib/test/budget.f measures for itself when the gate is not the caller.
: SUITE-TIMEOUT-MS ( -- n )
   SUITE-TIMEOUT-NOMINAL-MS T-BUDGET-MS ;

private

64 constant SUITE-USAGE-RC
0 constant SUITE-ALL-ID
2 constant SUITE-LINT-ID
3 constant SUITE-TOOL-ID
4 constant SUITE-CHECK-CLI-ID
5 constant SUITE-TAIL-ID
6 constant SUITE-LINT-TOOLS-ID
7 constant SUITE-LINT-LIBS-ID
8 constant SUITE-LINT-ARTIFACTS-ID
\ The proof slice: the parity gates that drive the Rocq proof assistant, plus the
\ register allocator's spill probe. They are minutes of work between them and
\ every one of them spawns children, so they get a slice of their own rather than
\ a place in a fast tier whose neighbours finish in a second.
9 constant SUITE-PROOF-ID

variable SUITE-SLICE
variable SUITE-SKIP-TOOL-LINTS
variable SUITE-SKIP-TOOL-REPAIR
variable SUITE-SKIP-TOOL-DOC
variable SUITE-SKIP-TOOL-TYPED
variable SUITE-ARG-I
variable SUITE-SLICE-SEEN
variable SUITE-TIMINGS

: SUITE-TRUE ( -- bool )
   0 0= ;

: SUITE-FALSE ( -- bool )
   0 0= 0= ;

: SUITE-USAGE ( -- )
   s" usage: test/gate-stdlib.f [lint|lint-tools|lint-artifacts|lint-libs|tool|check-cli|tail|proof] [--pool-slots N] [--timings]" SUITE-USAGE-RC die ;

: SUITE-ARG$ ( -- ptr u8 n )
   SUITE-ARG-I @ SCRIPT-ARGV$ ;

: SUITE-ARG-VALUE$ ( -- ptr u8 n )
   SUITE-ARG-I @ 1+ SCRIPT-ARGC >= if SUITE-USAGE then
   SUITE-ARG-I @ 1+ SCRIPT-ARGV$ ;

: SUITE-POS-NUM ( ptr u8 n -- n )
   STR>NUMBER? MATCH option
     none OF SUITE-USAGE ENDOF
     some OF ENDOF
   ;MATCH
   dup 1 < if drop SUITE-USAGE then ;

: SUITE-ADVANCE ( n -- )
   SUITE-ARG-I @ + SUITE-ARG-I ! ;

: SUITE-SLICE! ( n -- )
   SUITE-SLICE ! ;

: SUITE-SKIP-TOOL-LINTS! ( -- )
   -1 SUITE-SKIP-TOOL-LINTS ! ;

: SUITE-SKIP-TOOL-REPAIR! ( -- )
   -1 SUITE-SKIP-TOOL-REPAIR ! ;

: SUITE-SKIP-TOOL-DOC! ( -- )
   -1 SUITE-SKIP-TOOL-DOC ! ;

: SUITE-SKIP-TOOL-TYPED! ( -- )
   -1 SUITE-SKIP-TOOL-TYPED ! ;

public

: SKIP-SEMANTIC! ( -- )
   SUITE-SKIP-TOOL-LINTS!
   SUITE-SKIP-TOOL-REPAIR!
   SUITE-SKIP-TOOL-DOC!
   SUITE-SKIP-TOOL-TYPED! ;

private

: SUITE-POOL-OPT ( -- )
   SUITE-ARG-VALUE$ SUITE-POS-NUM GT-POOL-SLOTS!
   2 SUITE-ADVANCE ;

: SUITE-TIMINGS-OPT ( -- )
   -1 SUITE-TIMINGS !
   1 SUITE-ADVANCE ;

public

\ The slice a command-line token names. The gate's own argument parser is the
\ first consumer; test/run-lib.f names the same tokens in PHASE-SLICE-TOKEN when
\ it spawns a phase, and tools/lint/schedule-lint.f reads one and looks up the
\ other here, so a phase asking for a slice this gate does not have is a finding
\ instead of a usage exit nobody sees.
: SLICE-ID? ( ptr u8 n -- option<n> ) {: a:ptr u:n :}
   a u s" lint" STR= if SUITE-LINT-ID OPTION:SOME exit then
   a u s" lint-tools" STR= if SUITE-LINT-TOOLS-ID OPTION:SOME exit then
   a u s" lint-artifacts" STR= if SUITE-LINT-ARTIFACTS-ID OPTION:SOME exit then
   a u s" lint-libs" STR= if SUITE-LINT-LIBS-ID OPTION:SOME exit then
   a u s" tool" STR= if SUITE-TOOL-ID OPTION:SOME exit then
   a u s" check-cli" STR= if SUITE-CHECK-CLI-ID OPTION:SOME exit then
   a u s" tail" STR= if SUITE-TAIL-ID OPTION:SOME exit then
   a u s" proof" STR= if SUITE-PROOF-ID OPTION:SOME exit then
   OPTION:NONE ;

private

: SUITE-SLICE-ARG? ( -- bool )
   SUITE-ARG$ SLICE-ID? MATCH option
     none OF SUITE-FALSE exit ENDOF
     some OF ENDOF
   ;MATCH
   SUITE-SLICE! SUITE-TRUE ;

: SUITE-SLICE-OPT ( -- )
   SUITE-SLICE-SEEN @ if SUITE-USAGE then
   SUITE-SLICE-ARG? 0= if SUITE-USAGE then
   -1 SUITE-SLICE-SEEN !
   1 SUITE-ADVANCE ;

: SUITE-PARSE-ARG ( -- )
   SUITE-ARG$ s" --pool-slots" STR= if SUITE-POOL-OPT exit then
   SUITE-ARG$ s" --timings" STR= if SUITE-TIMINGS-OPT exit then
   SUITE-SLICE-OPT ;

: SUITE-PARSE-SLICE ( -- )
   SUITE-ALL-ID SUITE-SLICE!
   0 SUITE-SLICE-SEEN !
   0 SUITE-TIMINGS !
   0 SUITE-ARG-I !
   begin SUITE-ARG-I @ SCRIPT-ARGC < while
      SUITE-PARSE-ARG
   repeat ;

: SUITE-CHECK-ARGS ( -- )
   SUITE-PARSE-SLICE ;

: SUITE-ENV ( -- )
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING ;

: SUITE-RUN-ENV ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n timeout:n label:ptr labelu:n :}
   SUITE-ENV
   path pathu >LEN PROC-ARGV-CHECK-PATH
   PROC-CAPTURE-RESET
   timeout >MS PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   path pathu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-CAPTURE
   label labelu GT-PROGRESS-CAPTURE
   PROC-CLOSE-CAPTURE-FDS ;

: SUITE-RUN-STDIN ( ptr u8 n ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n in:ptr inu:n timeout:n label:ptr labelu:n :}
   SUITE-ENV
   path pathu >LEN PROC-ARGV-CHECK-PATH
   inu 0 < if E-PROC-OUTPUT throw then
   PROC-CAPTURE-RESET
   timeout >MS PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   PROC-SETUP-STDIN-FDS
   path pathu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE
   in inu >LEN label labelu GT-PROGRESS-STDIN-CAPTURE
   PROC-CLOSE-STDIN-FDS
   PROC-CLOSE-CAPTURE-FDS ;

: SUITE-RUN-ENV-ASYNC ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n timeout:n label:ptr labelu:n :}
   SUITE-ENV
   path pathu label labelu timeout GT-POOL-START ;

: SUITE-FAIL ( ptr u8 n -- ) {: label:ptr labelu:n :}
   s" FAIL: " type label labelu type cr
   s" rc: " type GT-RC@ . cr
   GT-OUT$ type
   GT-ERR$ type
   s" gate stdlib phase failed" 1 die ;

: SUITE-EXPECT-OK ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GT-RC@ 0 <> if label labelu SUITE-FAIL then ;

: SUITE-ARG+ ( ptr u8 n -- )
    >LEN PROC-ARGV+ ;

: SUITE-CLEANUP ( -- )
   ;

: SUITE-LABEL$ ( -- ptr u8 n )
   TEST:LABEL$ ;

: SUITE-LABEL= ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u TEST:LABEL= ;

: SUITE-ALL? ( -- bool )
   SUITE-SLICE @ SUITE-ALL-ID = ;

: SUITE-LINT-TOOLS-LABEL? ( -- bool )
   s" shadow-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" clobber-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" clobber-lint-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" repl-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" stdin-closure-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" gate-stats" SUITE-LABEL= if SUITE-TRUE exit then
   s" dot-dep-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" dot-dep-lint-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" maki-dep-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" maki-dep-lint-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" lint-def-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" namespace-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" namespace-lint-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" error-code-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" error-code-lint-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" lint-intern-set" SUITE-LABEL= if SUITE-TRUE exit then
   s" diff-parser" SUITE-LABEL= if SUITE-TRUE exit then
   s" diff-frame-codec" SUITE-LABEL= if SUITE-TRUE exit then
   s" schedule-lint" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-LINT-ARTIFACTS-LABEL? ( -- bool )
   s" imgdump-compare" SUITE-LABEL= if SUITE-TRUE exit then
   s" imagedisasm-tool" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-LINT-LIBS-LABEL? ( -- bool )
   s" string-helpers" SUITE-LABEL= if SUITE-TRUE exit then
   s" utf8-scalar" SUITE-LABEL= if SUITE-TRUE exit then
   s" array-helpers" SUITE-LABEL= if SUITE-TRUE exit then
   s" table-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" regex-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" map-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" ffi-abi" SUITE-LABEL= if SUITE-TRUE exit then
   s" ieee-float32" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-rep-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-tile-loop-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-tile-smem-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-tile-acc-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-tile-v4a-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-tile-pipe-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-gemm-checked-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-attention-checked-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-toolchain" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-LINT? ( -- bool )
   SUITE-SLICE @ case
      SUITE-LINT-ID of
         SUITE-LINT-TOOLS-LABEL? if SUITE-TRUE exit then
         SUITE-LINT-ARTIFACTS-LABEL? if SUITE-TRUE exit then
         SUITE-LINT-LIBS-LABEL?
      endof
      SUITE-LINT-TOOLS-ID of SUITE-LINT-TOOLS-LABEL? endof
      SUITE-LINT-ARTIFACTS-ID of SUITE-LINT-ARTIFACTS-LABEL? endof
      SUITE-LINT-LIBS-ID of SUITE-LINT-LIBS-LABEL? endof
      SUITE-FALSE swap
   endcase ;

: SUITE-TOOL? ( -- bool )
   SUITE-SLICE @ SUITE-TOOL-ID <> if SUITE-FALSE exit then
   s" tool-boundary-aot-call" SUITE-LABEL= if SUITE-TRUE exit then
   s" tool-boundary-check-repair" SUITE-LABEL= if
      SUITE-SKIP-TOOL-REPAIR @ 0= if SUITE-TRUE exit then
      SUITE-FALSE exit
   then
   s" tool-boundary-doc-public" SUITE-LABEL= if
      SUITE-SKIP-TOOL-DOC @ 0= if SUITE-TRUE exit then
      SUITE-FALSE exit
   then
   s" tool-boundary-lints" SUITE-LABEL= if
      SUITE-SKIP-TOOL-LINTS @ 0= if SUITE-TRUE exit then
      SUITE-FALSE exit
   then
   s" tool-boundary-typed-local" SUITE-LABEL= if
      SUITE-SKIP-TOOL-TYPED @ 0= if SUITE-TRUE exit then
      SUITE-FALSE exit
   then
   SUITE-FALSE ;

: SUITE-CHECK-CLI? ( -- bool )
   SUITE-SLICE @ SUITE-CHECK-CLI-ID <> if SUITE-FALSE exit then
   s" check-cli-boundary" SUITE-LABEL= ;

\ The tail slice runs one spawned child engine per suite through the pool, with
\ the 120s per-suite budget above. That is what the entries below need and what
\ the resident fast tier cannot give them: each boots child engines of its own,
\ writes snapshots, or drives a whole build.
: SUITE-TAIL-ENGINE? ( -- bool )
   s" pre-trust-defer" SUITE-LABEL= if SUITE-TRUE exit then
   s" top-row-hook" SUITE-LABEL= if SUITE-TRUE exit then
   s" checker-scan-index" SUITE-LABEL= if SUITE-TRUE exit then
   s" snapshot-writer" SUITE-LABEL= if SUITE-TRUE exit then
   s" stdlib-standalone-load" SUITE-LABEL= if SUITE-TRUE exit then
   s" aot-wid-restore" SUITE-LABEL= if SUITE-TRUE exit then
   s" aot-seed-batch" SUITE-LABEL= if SUITE-TRUE exit then
   s" aot-wide-format" SUITE-LABEL= if SUITE-TRUE exit then
   s" using-import" SUITE-LABEL= if SUITE-TRUE exit then
   s" load-reject-diag" SUITE-LABEL= if SUITE-TRUE exit then
   s" load-argv-contract" SUITE-LABEL= if SUITE-TRUE exit then
   s" engine-candidate-resolver" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-TAIL-BUILD? ( -- bool )
   s" build-fixpoint-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" boot-pin-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" hb-build-fixtures" SUITE-LABEL= ;

\ Suites that need to BE a top-level process, not a forked child of one.
\ codegen-fork-reference's first claim is that PROC-FORK:CHILD? is false where it
\ maps the clang reference column; tasking-threads creates pthreads, which do not
\ survive a gate-pool fork; and gate-budget starts the runner itself, so it needs
\ its own script arguments and its own runner state rather than the ones it would
\ inherit from a slice it was forked out of. This slice spawns one fresh process
\ per suite, which is exactly the shape all three ask for.
: SUITE-TAIL-PROCESS? ( -- bool )
   s" codegen-fork-reference" SUITE-LABEL= if SUITE-TRUE exit then
   s" gate-budget" SUITE-LABEL= if SUITE-TRUE exit then
   s" tasking-threads" SUITE-LABEL= ;

: SUITE-TAIL? ( -- bool )
   SUITE-SLICE @ SUITE-TAIL-ID <> if SUITE-FALSE exit then
   s" source-stdlib-stdin" SUITE-LABEL= if SUITE-TRUE exit then
   s" argv-stdlib-mocks" SUITE-LABEL= if SUITE-TRUE exit then
   s" argv-stdlib-script-args" SUITE-LABEL= if SUITE-TRUE exit then
   s" stdlib-source-default" SUITE-LABEL= if SUITE-TRUE exit then
   s" pointer-storage" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-TAIL-ENGINE? if SUITE-TRUE exit then
   SUITE-TAIL-PROCESS? if SUITE-TRUE exit then
   SUITE-TAIL-BUILD? ;

\ The Rocq parity gates and the spill probe. Each one compiles a formal model
\ with an external proof assistant or migrates definitions of its own, and the
\ measured walls run from 26s to over a minute and a half, so they own a slice
\ and a phase rather than sharing a fast tier's budget with second-long
\ neighbours. codegen-compare is deliberately NOT here: its three files are
\ scheduled through the resident tail-pure fork group instead.
: SUITE-PROOF? ( -- bool )
   SUITE-SLICE @ SUITE-PROOF-ID <> if SUITE-FALSE exit then
   s" compiler-ir-id-proof" SUITE-LABEL= if SUITE-TRUE exit then
   s" compiler-ir-intern-proof" SUITE-LABEL= if SUITE-TRUE exit then
   s" compiler-ir-structure-proof" SUITE-LABEL= if SUITE-TRUE exit then
   s" compiler-ir-storage-proof" SUITE-LABEL= if SUITE-TRUE exit then
   s" checker-model-proof" SUITE-LABEL= if SUITE-TRUE exit then
   s" compiler-reloc-proof" SUITE-LABEL= if SUITE-TRUE exit then
   s" compiler-insn-proof" SUITE-LABEL= if SUITE-TRUE exit then
   s" codegen-spill-probe" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-RUN? ( -- bool )
   SUITE-ALL? if SUITE-TRUE exit then
   SUITE-LINT? if SUITE-TRUE exit then
   SUITE-TOOL? if SUITE-TRUE exit then
   SUITE-CHECK-CLI? if SUITE-TRUE exit then
   SUITE-TAIL? if SUITE-TRUE exit then
   SUITE-PROOF? if SUITE-TRUE exit then
   SUITE-FALSE ;

\ ---- asking the selector about a label it has not reached ---------------------
\
\ The gate asks SUITE-RUN? once per registration, with the slice its arguments
\ chose and the label the suite registry has just made current. A tool that wants
\ the same answer about a registration the run has not reached - which slices, if
\ any, would run it - has to set those same two things and ask the same word. It
\ does NOT get to restate the rule: a second copy of "which labels does the tail
\ slice select" would agree with the first only until someone edited one of them.
\
\ Both settings are restored, so the probe is invisible to a caller mid-run.

$100 constant SUITE-PROBE-CAP           \ >= lib/test/suite.f NAME-CAP (128)
create SUITE-PROBE-BUF SUITE-PROBE-CAP allot
variable SUITE-PROBE-U

: SUITE-PROBE-SAVE ( -- )
   TEST:LABEL$ {: a:ptr u:n :}
   u SUITE-PROBE-CAP > if E-STR-CAPACITY throw then
   a SUITE-PROBE-BUF u BYTE-COPY
   u SUITE-PROBE-U ! ;

: SUITE-PROBE-RESTORE ( -- )
   SUITE-PROBE-BUF SUITE-PROBE-U @ TEST:LABEL! ;

public

: SLICE-SELECTS? ( ptr u8 n n -- bool ) {: a:ptr u:n slice:n :}
   SUITE-SLICE @ {: saved:n :}
   SUITE-PROBE-SAVE
   slice SUITE-SLICE!
   a u TEST:LABEL!
   SUITE-RUN? {: hit:bool :}
   saved SUITE-SLICE!
   SUITE-PROBE-RESTORE
   hit ;

private

: SUITE-HB ( -- )
   PROC-ARGV-RESET
   s" --load" SUITE-ARG+ ;

: SUITE-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then
   2dup EXECUTABLE? 0= if E-FS-OPEN throw then ;

: SUITE-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GS-INNER-HB-EVENT
   label labelu GS-BOUNDARY-EVENT
   SUITE-HB$ SUITE-TIMEOUT-MS label labelu SUITE-RUN-ENV-ASYNC ;

: SUITE-HB-RUN-STDIN ( ptr u8 n ptr u8 n -- ) {: in:ptr inu:n label:ptr labelu:n :}
   GT-POOL-DRAIN
   label labelu GT-PROGRESS-RUN
   label labelu GS-INNER-HB-STDIN-EVENT
   label labelu GS-BOUNDARY-EVENT
   SUITE-HB$ in inu SUITE-TIMEOUT-MS label labelu SUITE-RUN-STDIN
   label labelu SUITE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: SUITE-POOL-PASS-SPAN ( ptr u8 n n -- ) {: label:ptr labelu:n ms:n :}
   label labelu ms GS-SPAN-AUTH ;

: SUITE-INSTALL-POOL-HOOKS ( -- )
   [: SUITE-POOL-PASS-SPAN ;] is GT-POOL-PASS-HOOK ;

: SUITE-SETUP ( -- )
   SUITE-CHECK-ARGS
   GT-RESET
   GT-POOL-RESET
   SUITE-INSTALL-POOL-HOOKS ;

: SUITE-INSTALL-TEST-HOOKS ( -- )
   [: SUITE-SETUP ;] TEST:SETUP!
   [: SUITE-CLEANUP ;] TEST:TEARDOWN!
   [: GT-POOL-DRAIN ;] TEST:DRAIN!
   [: SUITE-HB ;] TEST:ARGS-BEGIN!
   [: SUITE-ARG+ ;] TEST:ARG+!
   [: SUITE-RUN? ;] TEST:SELECT?!
   [: SUITE-HB-RUN ;] TEST:RUNNER!
   [: SUITE-HB-RUN-STDIN ;] TEST:STDIN-RUNNER! ;

public

: MAIN ( -- )
   SUITE-INSTALL-TEST-HOOKS
   TEST:RESET ;

;package
