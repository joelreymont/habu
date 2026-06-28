\ run.f - checked native default gate runner.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ lib/fs-mutate.f, lib/process.f, lib/process-argv.f, lib/process-env.f,
\ lib/test-runner.f, and test/gate-pool.f.

include test/gate-stats.f

64 constant TR-USAGE-RC
65 constant TR-BUDGET-RC
90000 constant TR-DEFAULT-BUDGET-MS
600000 constant TR-TIMEOUT-MS
2 constant TR-WARM-PHASES
21 constant TR-PHASES
$2 constant TR-CHECK-WARM-PHASES
$D constant TR-LATE-PHASES
0 constant TR-TOOLS-WARM-SLOT
1 constant TR-CHECK-WARM-SLOT

\ Longest post-warm phases first; this keeps ARM gates inside budget without
\ dropping coverage or raising the threshold.
create TR-CHECK-WARM-ORDER
$9 , $E ,

create TR-LATE-ORDER
$3 , $2 , $A , $4 , $B , $C , $13 , $11 , $5 , $D , $14 , $12 , $10 ,

create TR-WARM-BUF FS-PATH-CAP allot
create TR-TOOLS-BUF FS-PATH-CAP allot
create TR-TOOLS-TRUST-BUF FS-PATH-CAP allot
create TR-BUILD-CACHE-BUF FS-PATH-CAP allot
create TR-PATH-BUF FS-PATH-CAP allot
create TR-UNDER-BUF FS-PATH-CAP allot
create TR-UNDER-HEX 64 allot

variable TR-WARM-U
variable TR-TOOLS-U
variable TR-TOOLS-TRUST-U
variable TR-BUILD-CACHE-U
variable TR-PATH-U
variable TR-UNDER-U
variable TR-GATE-START-NS
variable TR-TOOLS-WARM-READY
variable TR-CHECK-WARM-READY
variable TR-UNDER-READY

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

: TR-UNDER$ ( -- ptr u8 n )
   TR-UNDER-BUF TR-UNDER-U @ ;

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

: TR-GATE-START! ( -- )
   mono-ns TR-GATE-START-NS ! ;

: TR-GATE-ELAPSED-MS ( -- n )
   mono-ns TR-GATE-START-NS @ - PROC-NS-PER-MS / ;

: TR-BUDGET-CHECK ( n -- n ) {: budget:n :}
   budget 1 < if E-TBL-FIELD throw then
   budget ;

: TR-BUDGET-MS ( -- n )
   s" HABU_GATE_BUDGET_MS" GETENV dup 0= if
      2drop TR-DEFAULT-BUDGET-MS exit
   then
   STR>NUMBER? 0= if drop E-TBL-FIELD throw then
   TR-BUDGET-CHECK ;

: TR-BUDGET-FAIL ( n n -- ) {: elapsed:n budget:n :}
   s" FAIL: native gate budget (" type
   elapsed GT-U-TYPE
   s" ms > " type
   budget GT-U-TYPE
   s" ms)" type cr
   s" native gate budget exceeded" TR-BUDGET-RC die ;

: TR-PASS ( n n -- ) {: elapsed:n budget:n :}
   s" PASS: native gate (fixpoint + engine suite + checked hb + repl + hb-build) (" type
   elapsed GT-U-TYPE
   s" ms <= " type
   budget GT-U-TYPE
   s" ms budget)" type cr ;

: TR-FINISH ( -- )
   TR-GATE-ELAPSED-MS {: elapsed:n :}
   TR-BUDGET-MS {: budget:n :}
   elapsed budget > if elapsed budget TR-BUDGET-FAIL then
   elapsed budget TR-PASS ;

: TR-BUILD-CACHE-ENV ( -- )
   GT-ROOT s" hb-build-cache" TR-BUILD-CACHE-BUF JOIN-PATH TR-BUILD-CACHE-U !
   TR-BUILD-CACHE$ MAKE-DIRS
   s" HABU_BUILD_CACHE" >LEN TR-BUILD-CACHE$ >LEN PROC-ENV+ ;

: TR-UNDER-PATHS ( -- )
   GT-ROOT s" hb-under-test" TR-UNDER-BUF JOIN-PATH TR-UNDER-U !
   TR-UNDER$ EXISTS? if TR-UNDER$ REMOVE-FILE then
   0 TR-UNDER-READY ! ;

: TR-UNDER-ENV+ ( -- )
   s" HABU_UNDER_TEST" >LEN TR-UNDER$ >LEN PROC-ENV+ ;

: TR-START ( -- )
   GT-RESET
   CLEANUP-RESET
   s" HB_TMP" GETENV dup 0= if
      2drop
      s" hb-gate" TMPDIR-MKDIR GT-COPY-ROOT!
      GT-ROOT CLEANUP-TREE+
   else
      2dup MAKE-DIRS
      GT-COPY-ROOT!
   then
   GT-ROOT GS-ROOT!
   TR-UNDER-PATHS ;

: TR-FAIL ( ptr u8 n -- ) {: label:ptr labelu :}
   s" FAIL: " type label labelu type cr
   GT-CLEANUP
   label labelu 1 die ;

: TR-UNDER-SHA! ( -- )
   TR-UNDER$ TR-UNDER-HEX SHA256-FILE-HEX 0 <> if
      s" failed to hash Habu-under-test" TR-FAIL
   then ;

: TR-UNDER-LINE ( -- )
   TR-UNDER-SHA!
   s" Habu-under-test: " type
   TR-UNDER$ type
   s"  sha256=" type
   TR-UNDER-HEX 64 type cr ;

: TR-EXPECT-UNDER ( -- )
   TR-UNDER$ EXECUTABLE? 0= if
      s" missing Habu-under-test: " type TR-UNDER$ type cr
      s" Habu-under-test not produced executable" TR-FAIL
   then
   -1 TR-UNDER-READY !
   TR-UNDER-LINE ;

: TR-BASE ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HABU_GATE_WARM_PERSIST" GETENV dup 0= if 2drop else MAKE-DIRS then
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   s" HABU_GATE_WARM_ROOT" >LEN GT-ROOT >LEN PROC-ENV+
   TR-BUILD-CACHE-ENV
   GS-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/test-runner.f"  >LEN PROC-ARGV+ ;

: TR-SPAWN-CAPTURE ( -- )
   s" top-capture-spawn" GS-EVENT
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

\ Tools-warm root: the persistent HABU_GATE_WARM_PERSIST dir if the operator opted
\ in (content-stamped in gate-stdlib.f, so cross-run reuse is sound), else the
\ per-run GT-ROOT. Must match gate-stdlib.f SUITE-SET-ROOT so the baked image and
\ HABU_WARM_TOOLS resolve to the same place. The checker warm + per-run sharing keep
\ using GT-ROOT via HABU_GATE_WARM_ROOT.
: TR-WARM-ROOT$ ( -- ptr u8 n )
   s" HABU_GATE_WARM_PERSIST" GETENV dup 0= 0= if exit then
   2drop GT-ROOT ;

: TR-TOOLS-PATHS ( -- )
   TR-WARM-ROOT$ s" hb-tools-warm" TR-TOOLS-BUF JOIN-PATH TR-TOOLS-U !
   TR-TOOLS$ s" .trust.f" TR-TOOLS-TRUST-BUF TR-TOOLS-TRUST-U TR-SUFFIX! ;

: TR-TOOLS-ENV ( -- )
   TR-TOOLS-PATHS
   s" HABU_WARM_TOOLS" >LEN TR-TOOLS$ >LEN PROC-ENV+
   s" HABU_WARM_TOOLS_TRUST" >LEN TR-TOOLS-TRUST$ >LEN PROC-ENV+ ;

: TR-BUILD-COMMON ( -- )
   TR-COMMON
   s" test/gate-build-common.f"  >LEN PROC-ARGV+ ;

: TR-BUILD-LIB ( -- )
   s" lib/source.f"  >LEN PROC-ARGV+
   s" lib/build.f"  >LEN PROC-ARGV+
   s" lib/codesign.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" tools/warm-run.f"  >LEN PROC-ARGV+
   s" tools/hb-build-lib.f"  >LEN PROC-ARGV+ ;

: TR-BUILD-LIB-COMMON ( -- )
   TR-COMMON
   TR-BUILD-LIB
   s" test/gate-build-common.f"  >LEN PROC-ARGV+
   s" test/gate-build-hbb.f"  >LEN PROC-ARGV+ ;

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

: TR-STDLIB-LINT-TOOLS-ARGS ( -- )
   s" lint-tools" TR-STDLIB-SLICE-ARGS ;

: TR-STDLIB-LINT-MANIFEST-ARGS ( -- )
   s" tools/lint/text.f"  >LEN PROC-ARGV+
   s" tools/lint/token.f"  >LEN PROC-ARGV+
   s" tools/lint/lib.f"  >LEN PROC-ARGV+
   s" tools/stdlib-manifest-test.f"  >LEN PROC-ARGV+ ;

: TR-STDLIB-LINT-ARTIFACTS-ARGS ( -- )
   s" lint-artifacts" TR-STDLIB-SLICE-ARGS ;

: TR-STDLIB-LINT-LIBS-ARGS ( -- )
   s" lint-libs" TR-STDLIB-SLICE-ARGS ;

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
   s" test/gate-debug.f"  >LEN PROC-ARGV+ ;

: TR-AOT-POSITIVE-ARGS ( -- )
   TR-BUILD-LIB-COMMON
   s" test/gate-aot-positive.f"  >LEN PROC-ARGV+ ;

: TR-AOT-NEGATIVE-ARGS ( -- )
   TR-BUILD-COMMON
   s" test/gate-aot-negative.f"  >LEN PROC-ARGV+ ;

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
   s" native prop/debug gate phase" TR-RUN ;

: TR-AOT-POSITIVE ( -- )
   TR-BASE
   TR-AOT-POSITIVE-ARGS
   s" native hb-build AOT positive gate phase" TR-RUN ;

: TR-AOT-NEGATIVE ( -- )
   TR-BASE
   TR-AOT-NEGATIVE-ARGS
   s" native hb-build AOT negative gate phase" TR-RUN ;

: TR-PHASE-LABEL ( idx -- ptr u8 n ) {: idx :}
   idx IDX>N 0= if s" native stdlib tools warm image" exit then
   idx IDX>N 1 = if s" native checker warm image gate phase" exit then
   idx IDX>N 2 = if s" native stdlib tool-boundary slice" exit then
   idx IDX>N 3 = if s" native stdlib check-cli slice" exit then
   idx IDX>N 4 = if s" native stdlib tail slice" exit then
   idx IDX>N 5 = if s" native engine repair slice" exit then
   idx IDX>N 6 = if s" native prop/debug gate phase" exit then
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
   idx IDX>N 17 = if s" native stdlib lint tools slice" exit then
   idx IDX>N 18 = if s" native stdlib lint manifest slice" exit then
   idx IDX>N 19 = if s" native stdlib lint artifacts slice" exit then
   idx IDX>N 20 = if s" native stdlib lint libs slice" exit then
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
   idx IDX>N 17 = if s" gate-stdlib-lint-tools" exit then
   idx IDX>N 18 = if s" gate-stdlib-lint-manifest" exit then
   idx IDX>N 19 = if s" gate-stdlib-lint-artifacts" exit then
   idx IDX>N 20 = if s" gate-stdlib-lint-libs" exit then
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
   idx IDX>N 17 = if TR-STDLIB-LINT-TOOLS-ARGS exit then
   idx IDX>N 18 = if TR-STDLIB-LINT-MANIFEST-ARGS exit then
   idx IDX>N 19 = if TR-STDLIB-LINT-ARTIFACTS-ARGS exit then
   idx IDX>N 20 = if TR-STDLIB-LINT-LIBS-ARGS exit then
   E-TBL-BOUNDS throw ;

: TR-PHASE-TMP! ( idx -- ) {: idx :}
   GT-ROOT idx TR-PHASE-DIR TR-PATH-BUF JOIN-PATH TR-PATH-U !
   TR-PATH$ MAKE-DIRS ;

: TR-STDLIB-SLICE? ( idx -- bool ) {: idx :}
   idx IDX>N 2 >= idx IDX>N 4 <= and
   idx IDX>N 17 = or
   idx IDX>N 18 = or
   idx IDX>N 19 = or
   idx IDX>N 20 = or ;

: TR-TOOLS-PHASE? ( idx -- bool ) {: idx :}
   idx TR-STDLIB-SLICE? if 0 0= exit then
   idx IDX>N 5 = ;

: TR-EARLY-PHASE? ( idx -- bool ) {: idx :}
   idx IDX>N 6 = if 0 0= exit then
   idx IDX>N 7 = if 0 0= exit then
   idx IDX>N 8 = if 0 0= exit then
   idx IDX>N 15 = if 0 0= exit then
   0 0= 0= ;

: TR-NESTED-POOL-SLOTS$ ( -- ptr u8 n )
   s" 4" ;

: TR-PHASE-POOL-ENV ( idx -- ) {: idx :}
   idx TR-STDLIB-SLICE? if
      s" HABU_GATE_POOL_SLOTS" >LEN TR-NESTED-POOL-SLOTS$ >LEN PROC-ENV+
   then ;

: TR-PHASE-TOOLS-ENV ( idx -- ) {: idx :}
   idx TR-TOOLS-PHASE? if TR-TOOLS-ENV then ;

: TR-PHASE-UNDER-ENV? ( idx -- bool ) {: idx:idx :}
   idx IDX>N 15 = if 0 0= exit then
   TR-UNDER-READY @ 0 <> ;

: TR-PHASE-UNDER-EXE? ( idx -- bool ) {: idx:idx :}
   idx IDX>N 15 = if 0 0= 0= exit then
   TR-UNDER-READY @ 0 <> ;

: TR-PHASE-UNDER-ENV ( idx -- ) {: idx:idx :}
   idx TR-PHASE-UNDER-ENV? if
      s" under-env" GS-EVENT
      TR-UNDER-ENV+
   then ;

: TR-PHASE-EXE ( idx -- ptr u8 n ) {: idx:idx :}
   idx TR-PHASE-UNDER-EXE? if TR-UNDER$ exit then
   s" bin/hb" ;

: TR-PHASE-BASE ( idx -- ) {: idx :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   idx TR-PHASE-TMP!
   s" HB_TMP" >LEN TR-PATH$ >LEN PROC-ENV+
   s" HABU_GATE_WARM_ROOT" >LEN GT-ROOT >LEN PROC-ENV+
   idx TR-PHASE-TOOLS-ENV
   TR-BUILD-CACHE-ENV
   idx TR-PHASE-POOL-ENV
   GS-ENV+
   idx TR-PHASE-UNDER-ENV
   PROC-ENV-INHERIT-MISSING
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/test-runner.f"  >LEN PROC-ARGV+ ;

: TR-PHASE-START ( idx -- ) {: idx :}
   idx TR-PHASE-BASE
   idx TR-PHASE-ARGS
   s" top-phase-spawn" GS-EVENT
   idx TR-PHASE-UNDER-EXE? if s" under-phase-spawn" GS-EVENT then
   idx TR-PHASE-EXE idx TR-PHASE-LABEL TR-TIMEOUT-MS GT-POOL-START ;

: TR-PHASE-SPAWN-RANGE ( n n -- ) {: start end :}
   start begin dup end < while
      dup >IDX TR-PHASE-START
      1+
   repeat drop ;

: TR-WARM-READY-RESET ( -- )
   0 TR-TOOLS-WARM-READY !
   0 TR-CHECK-WARM-READY ! ;

: TR-WARM-READY-MARK ( -- )
   TR-TOOLS-WARM-READY @ 0= if
      TR-TOOLS-WARM-SLOT >IDX GT-POOL-DONE@ 0 <> if -1 TR-TOOLS-WARM-READY ! then
   then
   TR-CHECK-WARM-READY @ 0= if
      TR-CHECK-WARM-SLOT >IDX GT-POOL-DONE@ 0 <> if -1 TR-CHECK-WARM-READY ! then
   then ;

: TR-WARM-DONE? ( -- bool )
   TR-WARM-READY-MARK
   TR-TOOLS-WARM-READY @ 0 <>
   TR-CHECK-WARM-READY @ 0 <> and ;

: TR-UNDER-DONE? ( -- bool )
   TR-UNDER$ EXECUTABLE? ;

: TR-CHECK-WARM-DONE? ( -- bool )
   TR-WARM-READY-MARK
   TR-CHECK-WARM-READY @ 0 <> ;

: TR-DRAIN-UNTIL-UNDER ( -- )
   begin TR-UNDER-DONE? 0= while
      GT-POOL-STEP
   repeat
   TR-EXPECT-UNDER ;

: TR-DRAIN-UNTIL-WARM ( -- )
   begin TR-WARM-DONE? 0= while
      GT-POOL-STEP
   repeat ;

: TR-DRAIN-UNTIL-CHECK-WARM ( -- )
   begin TR-CHECK-WARM-DONE? 0= while
      GT-POOL-STEP
   repeat ;

: TR-CHECK-WARM-ORDER@ ( idx -- idx ) {: idx :}
   idx IDX>N cells TR-CHECK-WARM-ORDER + @ >IDX ;

: TR-LATE-ORDER@ ( idx -- idx ) {: idx :}
   idx IDX>N cells TR-LATE-ORDER + @ >IDX ;

: TR-EARLY-START ( -- )
   GT-POOL-RESET
   TR-WARM-READY-RESET
   0 TR-WARM-PHASES TR-PHASE-SPAWN-RANGE
   0 begin dup TR-PHASES < while
      dup >IDX TR-EARLY-PHASE? if dup >IDX TR-PHASE-START then
      1+
   repeat drop ;

: TR-LATE-START ( -- )
   0 begin dup TR-LATE-PHASES < while
      dup >IDX TR-LATE-ORDER@ TR-PHASE-START
      1+
   repeat drop ;

: TR-CHECK-WARM-START ( -- )
   0 begin dup TR-CHECK-WARM-PHASES < while
      dup >IDX TR-CHECK-WARM-ORDER@ TR-PHASE-START
      1+
   repeat drop ;

: TR-WORK-DRAIN ( -- )
   TR-LATE-START
   GT-POOL-DRAIN ;

: TR-DAG-RUN ( -- )
   TR-EARLY-START
   TR-DRAIN-UNTIL-UNDER
   TR-DRAIN-UNTIL-CHECK-WARM
   TR-CHECK-WARM-START
   TR-DRAIN-UNTIL-WARM
   TR-WORK-DRAIN ;

: TR-MAIN ( -- )
   TR-GATE-START!
   TR-CHECK-ARGS
   TR-START
   TR-CLEAN-WARM
   TR-EXPECT-HB
   TR-DAG-RUN
   GS-SUMMARY
   GT-CLEANUP
   TR-FINISH ;

TR-MAIN
