\ run.f - checked native default gate runner.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f, lib/process.f, lib/process-argv.f,
\ lib/process-env.f, lib/test-runner.f, and test/gate-pool.f.

64 constant TR-USAGE-RC
600000 constant TR-TIMEOUT-MS
2 constant TR-WARM-PHASES
19 constant TR-PHASES

create TR-WARM-BUF FS-PATH-CAP allot
create TR-TOOLS-BUF FS-PATH-CAP allot
create TR-TOOLS-TRUST-BUF FS-PATH-CAP allot
create TR-BUILD-CACHE-BUF FS-PATH-CAP allot
create TR-PATH-BUF FS-PATH-CAP allot

variable TR-WARM-U
variable TR-TOOLS-U
variable TR-TOOLS-TRUST-U
variable TR-BUILD-CACHE-U
variable TR-PATH-U

: TR-WARM$ ( -- ptr u8 n )
   TR-WARM-BUF TR-WARM-U @ ;

: TR-PATH$ ( -- ptr u8 n )
   TR-PATH-BUF TR-PATH-U @ ;

: TR-TOOLS$ ( -- ptr u8 n )
   TR-TOOLS-BUF TR-TOOLS-U @ ;

: TR-TOOLS-TRUST$ ( -- ptr u8 n )
   TR-TOOLS-TRUST-BUF TR-TOOLS-TRUST-U @ ;

: TR-BUILD-CACHE$ ( -- ptr u8 n )
   TR-BUILD-CACHE-BUF TR-BUILD-CACHE-U @ ;

: TR-USAGE ( -- )
   s" usage: bin/hb --load libs test/run.f" TR-USAGE-RC die ;

: TR-ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: TR-CHECK-ARGS ( -- )
   SCRIPT-ARGC 0= if exit then
   SCRIPT-ARGC 1 = s" full" TR-ARG0= and if
      s" test/run.f full retired; the native gate is test/run.f" TR-USAGE-RC die
   then
   TR-USAGE ;

: TR-BUILD-CACHE-ENV ( -- )
   GT-ROOT s" hb-build-cache" TR-BUILD-CACHE-BUF JOIN-PATH TR-BUILD-CACHE-U !
   TR-BUILD-CACHE$ MAKE-DIRS
   s" HABU_BUILD_CACHE" >LEN TR-BUILD-CACHE$ >LEN PROC-ENV+ ;

: TR-START ( -- )
   GT-RESET
   CLEANUP-RESET
   s" HB_TMP" GETENV dup 0= if
      2drop
      s" hb-gate" TMPDIR-MKDIR GT-COPY-ROOT!
      GT-ROOT CLEANUP-TREE+
      exit
   then
   2dup MAKE-DIRS
   GT-COPY-ROOT! ;

: TR-FAIL ( ptr u8 n -- ) {: label:ptr labelu :}
   s" FAIL: " type label labelu type cr
   GT-CLEANUP
   label labelu 1 die ;

: TR-BASE ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   s" HABU_GATE_WARM_ROOT" >LEN GT-ROOT >LEN PROC-ENV+
   TR-BUILD-CACHE-ENV
   PROC-ENV-INHERIT-MISSING
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/test-runner.f"  >LEN PROC-ARGV+ ;

: TR-SPAWN-CAPTURE ( -- )
   s" bin/hb" >LEN PROC-ARGV-CHECK-PATH
   PROC-CAPTURE-RESET
   TR-TIMEOUT-MS >MS PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   s" bin/hb" >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-CAPTURE ;

: TR-PHASE-OK? ( -- bool )
   PROC-OUTCOME-KIND @ PROC-OUTCOME-EXIT =
   PROC-OUTCOME-CODE @ 0= and ;

: TR-RUN ( ptr u8 n -- ) {: label:ptr labelu :}
   label labelu GT-PROGRESS-RUN
   TR-SPAWN-CAPTURE
   label labelu GT-PROGRESS-CAPTURE-FLUSH
   PROC-CLOSE-CAPTURE-FDS
   TR-PHASE-OK? 0= if label labelu TR-FAIL then
   label labelu GT-PROGRESS-PASS ;

: TR-COMMON ( -- )
   s" test/gate-common.f"  >LEN PROC-ARGV+ ;

: TR-CLEAN-WARM ( -- )
   GT-ROOT s" hb-check-warm" TR-WARM-BUF JOIN-PATH TR-WARM-U !
   TR-WARM$ FILE? if TR-WARM$ REMOVE-FILE then ;

: TR-SUFFIX! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u suf:ptr su dst:ptr lenp:ptr :}
   u su + FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   suf dst u + su BYTE-COPY
   u su + lenp ! ;

: TR-TOOLS-PATHS ( -- )
   GT-ROOT s" hb-tools-warm" TR-TOOLS-BUF JOIN-PATH TR-TOOLS-U !
   TR-TOOLS$ s" .trust.f" TR-TOOLS-TRUST-BUF TR-TOOLS-TRUST-U TR-SUFFIX! ;

: TR-TOOLS-ENV ( -- )
   TR-TOOLS-PATHS
   s" HABU_WARM_TOOLS" >LEN TR-TOOLS$ >LEN PROC-ENV+
   s" HABU_WARM_TOOLS_TRUST" >LEN TR-TOOLS-TRUST$ >LEN PROC-ENV+ ;

: TR-BUILD-COMMON ( -- )
   TR-COMMON
   s" test/gate-build-common.f"  >LEN PROC-ARGV+ ;

: TR-STDLIB-ARGS ( -- )
   s" test/gate-pool.f"  >LEN PROC-ARGV+
   s" test/gate-stdlib.f"  >LEN PROC-ARGV+ ;

: TR-STDLIB-SLICE-ARGS ( ptr u8 n -- ) {: slice:ptr sliceu :}
   TR-STDLIB-ARGS
   s" --"  >LEN PROC-ARGV+
   slice sliceu  >LEN PROC-ARGV+ ;

: TR-STDLIB-WARM-ARGS ( -- )
   s" warm" TR-STDLIB-SLICE-ARGS ;

: TR-STDLIB-LINT-ARGS ( -- )
   s" lint" TR-STDLIB-SLICE-ARGS ;

: TR-STDLIB-TOOL-ARGS ( -- )
   s" tool" TR-STDLIB-SLICE-ARGS ;

: TR-STDLIB-CHECK-CLI-ARGS ( -- )
   s" check-cli" TR-STDLIB-SLICE-ARGS ;

: TR-STDLIB-TAIL-ARGS ( -- )
   s" tail" TR-STDLIB-SLICE-ARGS ;

: TR-ENGINE-ARGS ( -- )
   TR-COMMON
   s" lib/build.f"  >LEN PROC-ARGV+
   s" lib/codesign.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" test/gate-engine.f"  >LEN PROC-ARGV+ ;

: TR-ENGINE-SLICE-ARGS ( ptr u8 n -- ) {: slice:ptr sliceu :}
   TR-ENGINE-ARGS
   s" --"  >LEN PROC-ARGV+
   slice sliceu  >LEN PROC-ARGV+ ;

: TR-ENGINE-BUILD-ARGS ( -- )
   s" build" TR-ENGINE-SLICE-ARGS ;

: TR-ENGINE-FIXTURES-ARGS ( -- )
   s" fixtures" TR-ENGINE-SLICE-ARGS ;

: TR-ENGINE-REPAIR-ARGS ( -- )
   s" repair" TR-ENGINE-SLICE-ARGS ;

: TR-ENGINE-RUNTIME-ARGS ( -- )
   s" runtime" TR-ENGINE-SLICE-ARGS ;

: TR-DICTIONARY-ARGS ( -- )
   TR-COMMON
   s" test/gate-dictionary.f"  >LEN PROC-ARGV+ ;

: TR-DIAGNOSTICS-ARGS ( -- )
   TR-COMMON
   s" test/gate-diagnostics.f"  >LEN PROC-ARGV+ ;

: TR-DIAG-SLICE-ARGS ( ptr u8 n -- ) {: slice:ptr sliceu :}
   TR-DIAGNOSTICS-ARGS
   s" --"  >LEN PROC-ARGV+
   slice sliceu  >LEN PROC-ARGV+ ;

: TR-DIAG-WARM-ARGS ( -- )
   s" warm" TR-DIAG-SLICE-ARGS ;

: TR-DIAG-REPAIR-ARGS ( -- )
   s" diag-repair" TR-DIAG-SLICE-ARGS ;

: TR-DIAG-UNDEF-PRIMARY-ARGS ( -- )
   s" diag-undef-primary" TR-DIAG-SLICE-ARGS ;

: TR-DIAG-ALL-STRICT-ARGS ( -- )
   s" diag-all-strict" TR-DIAG-SLICE-ARGS ;

: TR-DIAG-FILE-UNSAFE-ARGS ( -- )
   s" diag-file-unsafe" TR-DIAG-SLICE-ARGS ;

: TR-DEBUG-ARGS ( -- )
   TR-COMMON
   s" lib/codesign.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" test/gate-debug.f"  >LEN PROC-ARGV+ ;

: TR-AOT-POSITIVE-ARGS ( -- )
   TR-BUILD-COMMON
   s" test/gate-aot-positive.f"  >LEN PROC-ARGV+ ;

: TR-AOT-NEGATIVE-ARGS ( -- )
   TR-BUILD-COMMON
   s" test/gate-aot-negative.f"  >LEN PROC-ARGV+ ;

: TR-HB-BUILD-REPL-ARGS ( -- )
   TR-BUILD-COMMON
   s" test/gate-hb-build-repl.f"  >LEN PROC-ARGV+ ;

: TR-STDLIB ( -- )
   TR-BASE
   TR-STDLIB-ARGS
   s" native lint/stdlib gate phase" TR-RUN ;

: TR-ENGINE ( -- )
   TR-BASE
   TR-ENGINE-ARGS
   s" native engine gate phase" TR-RUN ;

: TR-EXPECT-HB ( -- )
   s" bin/hb" EXECUTABLE? 0= if s" bin/hb not produced executable" TR-FAIL then ;

: TR-DICTIONARY ( -- )
   TR-BASE
   TR-DICTIONARY-ARGS
   s" native dictionary/checker gate phase" TR-RUN ;

: TR-DIAGNOSTICS ( -- )
   TR-BASE
   TR-DIAGNOSTICS-ARGS
   s" native checker diagnostics gate phase" TR-RUN ;

: TR-DIAG-WARM ( -- )
   TR-BASE
   TR-DIAG-WARM-ARGS
   s" native checker warm image gate phase" TR-RUN ;

: TR-DEBUG ( -- )
   TR-BASE
   TR-DEBUG-ARGS
   s" native prop/snapshot/debug gate phase" TR-RUN ;

: TR-AOT-POSITIVE ( -- )
   TR-BASE
   TR-AOT-POSITIVE-ARGS
   s" native hb-build AOT positive gate phase" TR-RUN ;

: TR-AOT-NEGATIVE ( -- )
   TR-BASE
   TR-AOT-NEGATIVE-ARGS
   s" native hb-build AOT negative gate phase" TR-RUN ;

: TR-HB-BUILD-REPL ( -- )
   TR-BASE
   TR-HB-BUILD-REPL-ARGS
   s" native hb-build REPL gate phase" TR-RUN ;

: TR-PHASE-LABEL ( idx -- ptr u8 n ) {: idx :}
   idx IDX>N 0= if s" native stdlib tools warm image" exit then
   idx IDX>N 1 = if s" native checker warm image gate phase" exit then
   idx IDX>N 2 = if s" native stdlib tool-boundary slice" exit then
   idx IDX>N 3 = if s" native stdlib check-cli slice" exit then
   idx IDX>N 4 = if s" native stdlib tail slice" exit then
   idx IDX>N 5 = if s" native engine repair slice" exit then
   idx IDX>N 6 = if s" native prop/snapshot/debug gate phase" exit then
   idx IDX>N 7 = if s" native hb-build AOT positive gate phase" exit then
   idx IDX>N 8 = if s" native hb-build AOT negative gate phase" exit then
   idx IDX>N 9 = if s" native engine fixture slice" exit then
   idx IDX>N 10 = if s" native checker diagnostics repair slice" exit then
   idx IDX>N 11 = if s" native checker diagnostics undef-primary slice" exit then
   idx IDX>N 12 = if s" native checker diagnostics all-strict slice" exit then
   idx IDX>N 13 = if s" native checker diagnostics file-unsafe slice" exit then
   idx IDX>N 14 = if s" native dictionary/checker gate phase" exit then
   idx IDX>N 15 = if s" native engine build slice" exit then
   idx IDX>N 16 = if s" native engine runtime slice" exit then
   idx IDX>N 17 = if s" native hb-build REPL gate phase" exit then
   idx IDX>N 18 = if s" native stdlib lint slice" exit then
   E-TBL-BOUNDS throw ;

: TR-PHASE-DIR ( idx -- ptr u8 n ) {: idx :}
   idx IDX>N 0= if s" gate-stdlib-warm" exit then
   idx IDX>N 1 = if s" gate-check-warm" exit then
   idx IDX>N 2 = if s" gate-stdlib-tool" exit then
   idx IDX>N 3 = if s" gate-stdlib-check-cli" exit then
   idx IDX>N 4 = if s" gate-stdlib-tail" exit then
   idx IDX>N 5 = if s" gate-engine-repair" exit then
   idx IDX>N 6 = if s" gate-debug" exit then
   idx IDX>N 7 = if s" gate-aot-pos" exit then
   idx IDX>N 8 = if s" gate-aot-neg" exit then
   idx IDX>N 9 = if s" gate-engine-fixtures" exit then
   idx IDX>N 10 = if s" gate-diag-repair" exit then
   idx IDX>N 11 = if s" gate-diag-undef-primary" exit then
   idx IDX>N 12 = if s" gate-diag-all-strict" exit then
   idx IDX>N 13 = if s" gate-diag-file-unsafe" exit then
   idx IDX>N 14 = if s" gate-dict" exit then
   idx IDX>N 15 = if s" gate-engine-build" exit then
   idx IDX>N 16 = if s" gate-engine-runtime" exit then
   idx IDX>N 17 = if s" gate-repl" exit then
   idx IDX>N 18 = if s" gate-stdlib-lint" exit then
   E-TBL-BOUNDS throw ;

: TR-PHASE-ARGS ( idx -- ) {: idx :}
   idx IDX>N 0= if TR-STDLIB-WARM-ARGS exit then
   idx IDX>N 1 = if TR-DIAG-WARM-ARGS exit then
   idx IDX>N 2 = if TR-STDLIB-TOOL-ARGS exit then
   idx IDX>N 3 = if TR-STDLIB-CHECK-CLI-ARGS exit then
   idx IDX>N 4 = if TR-STDLIB-TAIL-ARGS exit then
   idx IDX>N 5 = if TR-ENGINE-REPAIR-ARGS exit then
   idx IDX>N 6 = if TR-DEBUG-ARGS exit then
   idx IDX>N 7 = if TR-AOT-POSITIVE-ARGS exit then
   idx IDX>N 8 = if TR-AOT-NEGATIVE-ARGS exit then
   idx IDX>N 9 = if TR-ENGINE-FIXTURES-ARGS exit then
   idx IDX>N 10 = if TR-DIAG-REPAIR-ARGS exit then
   idx IDX>N 11 = if TR-DIAG-UNDEF-PRIMARY-ARGS exit then
   idx IDX>N 12 = if TR-DIAG-ALL-STRICT-ARGS exit then
   idx IDX>N 13 = if TR-DIAG-FILE-UNSAFE-ARGS exit then
   idx IDX>N 14 = if TR-DICTIONARY-ARGS exit then
   idx IDX>N 15 = if TR-ENGINE-BUILD-ARGS exit then
   idx IDX>N 16 = if TR-ENGINE-RUNTIME-ARGS exit then
   idx IDX>N 17 = if TR-HB-BUILD-REPL-ARGS exit then
   idx IDX>N 18 = if TR-STDLIB-LINT-ARGS exit then
   E-TBL-BOUNDS throw ;

: TR-PHASE-TMP! ( idx -- ) {: idx :}
   GT-ROOT idx TR-PHASE-DIR TR-PATH-BUF JOIN-PATH TR-PATH-U !
   TR-PATH$ MAKE-DIRS ;

: TR-STDLIB-SLICE? ( idx -- bool ) {: idx :}
   idx IDX>N 2 >= idx IDX>N 4 <= and
   idx IDX>N 18 = or ;

: TR-NESTED-POOL-SLOTS$ ( -- ptr u8 n )
   s" 2" ;

: TR-PHASE-POOL-ENV ( idx -- ) {: idx :}
   idx TR-STDLIB-SLICE? if
      s" HABU_GATE_POOL_SLOTS" >LEN TR-NESTED-POOL-SLOTS$ >LEN PROC-ENV+
   then ;

: TR-PHASE-BASE ( idx -- ) {: idx :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   idx TR-PHASE-TMP!
   s" HB_TMP" >LEN TR-PATH$ >LEN PROC-ENV+
   s" HABU_GATE_WARM_ROOT" >LEN GT-ROOT >LEN PROC-ENV+
   TR-TOOLS-ENV
   TR-BUILD-CACHE-ENV
   idx TR-PHASE-POOL-ENV
   PROC-ENV-INHERIT-MISSING
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/test-runner.f"  >LEN PROC-ARGV+ ;

: TR-PHASE-START ( idx -- ) {: idx :}
   idx TR-PHASE-BASE
   idx TR-PHASE-ARGS
   s" bin/hb" idx TR-PHASE-LABEL TR-TIMEOUT-MS GT-POOL-START ;

: TR-PHASE-SPAWN-RANGE ( n n -- ) {: start end :}
   GT-POOL-RESET
   start begin dup end < while
      dup >IDX TR-PHASE-START
      1+
   repeat drop ;

: TR-WARM-DRAIN ( -- )
   0 TR-WARM-PHASES TR-PHASE-SPAWN-RANGE
   GT-POOL-DRAIN ;

: TR-WORK-DRAIN ( -- )
   TR-WARM-PHASES TR-PHASES TR-PHASE-SPAWN-RANGE
   GT-POOL-DRAIN ;

: TR-DAG-RUN ( -- )
   TR-WARM-DRAIN
   TR-WORK-DRAIN ;

: TR-MAIN ( -- )
   TR-CHECK-ARGS
   TR-START
   TR-CLEAN-WARM
   TR-EXPECT-HB
   TR-DAG-RUN
   GT-CLEANUP
   s" PASS: native gate (fixpoint + engine suite + checked hb + repl + hb-build)" type cr ;

TR-MAIN
