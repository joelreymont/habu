\ run.f - checked native default gate runner.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ lib/fs-mutate.f, lib/process.f, lib/process-argv.f, lib/process-env.f,
\ lib/test-runner.f, and test/gate-pool.f.

include lib/content-key.f
include test/gate-stats.f

64 constant TR-USAGE-RC
65 constant TR-BUDGET-RC
90000 constant TR-DEFAULT-BUDGET-MS
600000 constant TR-TIMEOUT-MS
21 constant TR-PHASES
$2 constant TR-CHECK-WARM-PHASES
$F constant TR-LATE-PHASES
9 constant TR-UNDER-PREFIX-U
0 constant TR-TOOLS-WARM-SLOT
1 constant TR-CHECK-WARM-SLOT
2 constant TR-AOT-RUNNER-SLOT
3 constant TR-RUNNER-WARM-SLOT

\ Longest post-warm phases first; this keeps ARM gates inside budget without
\ dropping coverage or raising the threshold.
create TR-CHECK-WARM-ORDER
$9 , $E ,

create TR-LATE-ORDER
$3 , $2 , $8 , $7 , $A , $4 , $B , $C , $13 , $11 , $5 , $D , $14 , $12 , $10 ,

create TR-WARM-BUF FS-PATH-CAP allot
create TR-TOOLS-BUF FS-PATH-CAP allot
create TR-TOOLS-TRUST-BUF FS-PATH-CAP allot
create TR-CHECK-BUF FS-PATH-CAP allot
create TR-CHECK-TRUST-BUF FS-PATH-CAP allot
create TR-BUILD-CACHE-BUF FS-PATH-CAP allot
create TR-PATH-BUF FS-PATH-CAP allot
create TR-UNDER-BUF FS-PATH-CAP allot
create TR-RUNNER-BUF FS-PATH-CAP allot
create TR-RUNNER-TRUST-BUF FS-PATH-CAP allot
create TR-RUNNER-STAMP-BUF FS-PATH-CAP allot
create TR-AOT-RUNNER-BUF FS-PATH-CAP allot
create TR-AOT-RUNNER-TRUST-BUF FS-PATH-CAP allot
create TR-AOT-RUNNER-STAMP-BUF FS-PATH-CAP allot
create TR-UNDER-HEX 64 allot
create TR-UNDER-KEY-HEX 80 allot
create TR-UNDER-CACHE-BUF FS-PATH-CAP allot
create TR-UNDER-CACHE-TMP-BUF FS-PATH-CAP allot
create TR-UNDER-CACHE-LOCK-BUF FS-PATH-CAP allot
create TR-UNDER-NAME-BUF 80 allot
create TR-RUNNER-KEY-HEX 80 allot
create TR-RUNNER-STAMP-RD 80 allot
create TR-AOT-RUNNER-KEY-HEX 80 allot
create TR-AOT-RUNNER-STAMP-RD 80 allot

variable TR-WARM-U
variable TR-TOOLS-U
variable TR-TOOLS-TRUST-U
variable TR-CHECK-U
variable TR-CHECK-TRUST-U
variable TR-BUILD-CACHE-U
variable TR-PATH-U
variable TR-UNDER-U
variable TR-RUNNER-U
variable TR-RUNNER-TRUST-U
variable TR-RUNNER-STAMP-U
variable TR-AOT-RUNNER-U
variable TR-AOT-RUNNER-TRUST-U
variable TR-AOT-RUNNER-STAMP-U
variable TR-UNDER-CACHE-U
variable TR-UNDER-CACHE-TMP-U
variable TR-UNDER-CACHE-LOCK-U
variable TR-UNDER-NAME-U
variable TR-GATE-START-NS
variable TR-TOOLS-WARM-READY
variable TR-CHECK-WARM-READY
variable TR-UNDER-READY
variable TR-MANIFEST-EARLY
variable TR-LIBS-EARLY
variable TR-RUNNER-READY
variable TR-AOT-RUNNER-READY
variable TR-UNDER-CACHE-HIT
variable TR-UNDER-CACHE-RC

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

: TR-RUNNER$ ( -- ptr u8 n )
   TR-RUNNER-BUF TR-RUNNER-U @ ;

: TR-RUNNER-TRUST$ ( -- ptr u8 n )
   TR-RUNNER-TRUST-BUF TR-RUNNER-TRUST-U @ ;

: TR-RUNNER-STAMP$ ( -- ptr u8 n )
   TR-RUNNER-STAMP-BUF TR-RUNNER-STAMP-U @ ;

: TR-AOT-RUNNER$ ( -- ptr u8 n )
   TR-AOT-RUNNER-BUF TR-AOT-RUNNER-U @ ;

: TR-AOT-RUNNER-TRUST$ ( -- ptr u8 n )
   TR-AOT-RUNNER-TRUST-BUF TR-AOT-RUNNER-TRUST-U @ ;

: TR-AOT-RUNNER-STAMP$ ( -- ptr u8 n )
   TR-AOT-RUNNER-STAMP-BUF TR-AOT-RUNNER-STAMP-U @ ;

: TR-UNDER-CACHE$ ( -- ptr u8 n )
   TR-UNDER-CACHE-BUF TR-UNDER-CACHE-U @ ;

: TR-UNDER-CACHE-TMP$ ( -- ptr u8 n )
   TR-UNDER-CACHE-TMP-BUF TR-UNDER-CACHE-TMP-U @ ;

: TR-UNDER-CACHE-LOCK$ ( -- ptr u8 n )
   TR-UNDER-CACHE-LOCK-BUF TR-UNDER-CACHE-LOCK-U @ ;

: TR-UNDER-NAME$ ( -- ptr u8 n )
   TR-UNDER-NAME-BUF TR-UNDER-NAME-U @ ;

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

: TR-TRUE ( -- bool )
   0 0= ;

: TR-FALSE ( -- bool )
   TR-TRUE 0= ;

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

: TR-PERSIST? ( -- bool )
   s" HABU_GATE_WARM_PERSIST" GETENV dup 0= if
      2drop TR-FALSE exit
   then
   2drop TR-TRUE ;

: TR-PERSIST$ ( -- ptr u8 n )
   s" HABU_GATE_WARM_PERSIST" GETENV dup 0= if
      2drop E-FS-PATH throw
   then ;

: TR-PERSIST-ENSURE ( -- )
   s" HABU_GATE_WARM_PERSIST" GETENV dup 0= if
      2drop exit
   then
   MAKE-DIRS ;

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
   TR-PERSIST? if
      TR-PERSIST$ s" hb-build-cache" TR-BUILD-CACHE-BUF JOIN-PATH TR-BUILD-CACHE-U !
   else
      GT-ROOT s" hb-build-cache" TR-BUILD-CACHE-BUF JOIN-PATH TR-BUILD-CACHE-U !
   then
   TR-BUILD-CACHE$ MAKE-DIRS
   s" HABU_BUILD_CACHE" >LEN TR-BUILD-CACHE$ >LEN PROC-ENV+ ;

: TR-UNDER-PATHS ( -- )
   GT-ROOT s" hb-under-test" TR-UNDER-BUF JOIN-PATH TR-UNDER-U !
   TR-UNDER$ EXISTS? if TR-UNDER$ REMOVE-FILE then
   0 TR-UNDER-READY !
   0 TR-MANIFEST-EARLY !
   0 TR-LIBS-EARLY !
   0 TR-UNDER-CACHE-HIT !
   0 TR-RUNNER-READY !
   0 TR-AOT-RUNNER-READY ! ;

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
   TR-PERSIST-ENSURE
   GT-ROOT GS-ROOT!
   TR-UNDER-PATHS ;

: TR-FAIL ( ptr u8 n -- ) {: label:ptr labelu:n :}
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
   TR-PERSIST-ENSURE
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

: TR-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   TR-SPAWN-CAPTURE
   label labelu GT-PROGRESS-CAPTURE-FLUSH
   PROC-CLOSE-CAPTURE-FDS
   TR-PHASE-OK? 0= if label labelu TR-FAIL then
   label labelu GT-PROGRESS-PASS ;

: TR-COMMON ( -- )
   s" test/gate-common.f"  >LEN PROC-ARGV+ ;

: TR-BUILD-ASSERT-LIBS ( -- )
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/gate-json-assert-core.f"  >LEN PROC-ARGV+
   s" tools/aot-call-report.f"  >LEN PROC-ARGV+ ;

: TR-CLEAN-WARM ( -- )
   GT-ROOT s" hb-check-warm" TR-WARM-BUF JOIN-PATH TR-WARM-U !
   TR-WARM$ FILE? if TR-WARM$ REMOVE-FILE then ;

: TR-SUFFIX! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n suf:ptr su:n dst:ptr lenp:ptr :}
   u su + FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   suf dst u + su BYTE-COPY
   u su + lenp ! ;

: TR-FILES-END? ( ptr u8 n -- bool )
   s" ;TR-FILES" STR= ;

: TR-FILES-ITEM, ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   u c,
   0 begin dup u < while
      dup a + c@ c,
      1+
   repeat drop ;

: TR-FILES-PARSE ( -- )
   begin
      parse-name dup 0= if 2drop E-STR-BOUNDS throw then
      2dup TR-FILES-END? if 2drop 0 c, exit then
      TR-FILES-ITEM,
   again ;

\ typed-local-lint: allow-bare-local - q keeps the quotation effect from the stack signature.
: TR-FILES-WALK ( ptr a [ ptr u8 n -- ] -- ) {: p:ptr q :}
   p begin dup c@ 0= 0= while
      dup 1+ over c@ q execute
      dup c@ 1 + +
   repeat drop ;

: TR-FILES-RUN ( [ ptr u8 n -- ] ptr a -- )
   swap TR-FILES-WALK ;

: TR-FILES: ( -- )
   create TR-FILES-PARSE
   does> ( [ ptr u8 n -- ] -- )
      TR-FILES-RUN ;

TR-FILES: TR-RUNNER-SUPPORT-FILES
   lib/errors.f lib/string.f lib/memory.f lib/vector.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f
   lib/source.f lib/build.f lib/codesign.f lib/content-key.f tools/build-fixpoint.f
   tools/warm-run.f tools/hb-build-lib.f tools/json.f tools/gate-json-assert-core.f
   test/gate-pool.f test/gate-stats.f test/gate-common-lib.f test/gate-stdlib-lib.f test/gate-engine-lib.f
   test/gate-diagnostics-lib.f test/gate-dictionary-lib.f test/gate-debug-lib.f
;TR-FILES

TR-FILES: TR-AOT-RUNNER-SUPPORT-FILES
   lib/errors.f lib/string.f lib/memory.f lib/vector.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f
   lib/source.f lib/build.f lib/codesign.f lib/content-key.f tools/build-fixpoint.f
   tools/warm-run.f tools/hb-build-lib.f tools/lint/text.f tools/lint/token.f
   tools/lint/lib.f tools/lint/json-writer.f tools/lint/source-lex.f
   tools/aot-lint-core.f tools/signature-lint-core.f tools/hb-build-direct-lints.f
   tools/json.f tools/gate-json-assert-core.f tools/aot-call-report.f
   test/gate-stats.f test/gate-common-lib.f test/gate-build-common.f
   test/gate-build-hbb.f test/gate-aot-positive-lib.f test/gate-aot-negative-lib.f
;TR-FILES

TR-FILES: TR-UNDER-SOURCE-FILES
   tools/build-fixpoint.f test/gate-pool.f test/gate-common-lib.f
   test/gate-engine-lib.f test/gate-engine.f src/habu/hide.f
   src/core/util.f src/core/checker.f
   src/core/render.f src/core/check-hook.f src/core/roles.f
   src/arch/arm64/asm.f src/arch/arm64/icode.f src/arch/arm64/mnem.f
   src/habu/layout.f src/os/env-base.f src/os/script-argv.f
   src/core/structures.f src/core/enums.f src/core/exec-vector.f
   src/core/sha256.f src/core/combinators.f src/habu/treeshake.f
   src/habu/rt.f src/habu/crash.f src/os/image-bytes.f src/habu/habu1.f
   src/habu/prof.f src/habu/regalloc.f src/habu/jit.f src/habu/habu2.f
   src/habu/xref.f src/habu/driver-io.f src/core/include.f
   src/habu/stage2.f src/habu/stdin.f src/habu/snap.f src/habu/repl.f
   src/habu/debug-watch.f src/habu/stepper.f src/habu/debug.f
;TR-FILES

\ Tools-warm root: the persistent HABU_GATE_WARM_PERSIST dir if the operator opted
\ in (content-stamped in gate-stdlib.f, so cross-run reuse is sound), else the
\ per-run GT-ROOT. Must match gate-stdlib.f SUITE-SET-ROOT so the baked image and
\ HABU_WARM_TOOLS resolve to the same place. Checker warm uses the same persistent
\ root through GE-WARM-ROOT and validates with its own content stamp.
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

: TR-CHECK$ ( -- ptr u8 n )
   TR-CHECK-BUF TR-CHECK-U @ ;

: TR-CHECK-TRUST$ ( -- ptr u8 n )
   TR-CHECK-TRUST-BUF TR-CHECK-TRUST-U @ ;

: TR-CHECK-PATHS ( -- )
   TR-WARM-ROOT$ s" hb-check-warm" TR-CHECK-BUF JOIN-PATH TR-CHECK-U !
   TR-CHECK$ s" .trust.f" TR-CHECK-TRUST-BUF TR-CHECK-TRUST-U TR-SUFFIX! ;

: TR-CHECK-ENV ( -- )
   TR-CHECK-PATHS
   s" HABU_WARM_CHECK" >LEN TR-CHECK$ >LEN PROC-ENV+
   s" HABU_WARM_CHECK_TRUST" >LEN TR-CHECK-TRUST$ >LEN PROC-ENV+ ;

: TR-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: TR-RUNNER-PATHS ( -- )
   TR-WARM-ROOT$ s" hb-gate-warm" TR-RUNNER-BUF JOIN-PATH TR-RUNNER-U !
   TR-RUNNER$ s" .trust.f" TR-RUNNER-TRUST-BUF TR-RUNNER-TRUST-U TR-SUFFIX!
   TR-RUNNER$ s" .stamp" TR-RUNNER-STAMP-BUF TR-RUNNER-STAMP-U TR-SUFFIX! ;

: TR-AOT-RUNNER-PATHS ( -- )
   TR-WARM-ROOT$ s" hb-aot-warm" TR-AOT-RUNNER-BUF JOIN-PATH TR-AOT-RUNNER-U !
   TR-AOT-RUNNER$ s" .trust.f" TR-AOT-RUNNER-TRUST-BUF TR-AOT-RUNNER-TRUST-U TR-SUFFIX!
   TR-AOT-RUNNER$ s" .stamp" TR-AOT-RUNNER-STAMP-BUF TR-AOT-RUNNER-STAMP-U TR-SUFFIX! ;

: TR-KEY-FILE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CK-FILE+ ;

: TR-RUNNER-KEY-FILE+ ( ptr u8 n -- )
   TR-KEY-FILE+ ;

: TR-RUNNER-KEY-SUPPORT ( -- )
   [: TR-RUNNER-KEY-FILE+ ;] TR-RUNNER-SUPPORT-FILES ;

: TR-RUNNER-KEY! ( -- )
   CK-RESET
   s" hb-gate-runner-cache-v4" CK-TEXT+
   s" bin/hb" TR-RUNNER-KEY-FILE+
   s" tools/warm-image-lib.f" TR-RUNNER-KEY-FILE+
   s" tools/warm-image.f" TR-RUNNER-KEY-FILE+
   s" tools/public-signatures-core.f" TR-RUNNER-KEY-FILE+
   s" tools/public-signatures.f" TR-RUNNER-KEY-FILE+
   s" lib/content-key.f" TR-RUNNER-KEY-FILE+
   s" test/gate-runner-entry.f" TR-RUNNER-KEY-FILE+
   s" test/gate-stdlib-cases.f" TR-RUNNER-KEY-FILE+
   TR-RUNNER-KEY-SUPPORT
   TR-RUNNER-KEY-HEX CK-FINAL-HEX ;

: TR-AOT-RUNNER-KEY-FILE+ ( ptr u8 n -- )
   TR-KEY-FILE+ ;

: TR-AOT-RUNNER-KEY-SUPPORT ( -- )
   [: TR-AOT-RUNNER-KEY-FILE+ ;] TR-AOT-RUNNER-SUPPORT-FILES ;

: TR-AOT-RUNNER-KEY! ( -- )
   CK-RESET
   s" hb-aot-runner-cache-v1" CK-TEXT+
   s" bin/hb" TR-AOT-RUNNER-KEY-FILE+
   s" tools/warm-image-lib.f" TR-AOT-RUNNER-KEY-FILE+
   s" tools/warm-image.f" TR-AOT-RUNNER-KEY-FILE+
   s" tools/public-signatures-core.f" TR-AOT-RUNNER-KEY-FILE+
   s" tools/public-signatures.f" TR-AOT-RUNNER-KEY-FILE+
   s" lib/content-key.f" TR-AOT-RUNNER-KEY-FILE+
   s" test/gate-aot-runner-entry.f" TR-AOT-RUNNER-KEY-FILE+
   TR-AOT-RUNNER-KEY-SUPPORT
   TR-AOT-RUNNER-KEY-HEX CK-FINAL-HEX ;

: TR-UNDER-SOURCE-KEY ( -- )
   [: TR-KEY-FILE+ ;] TR-RUNNER-SUPPORT-FILES
   [: TR-KEY-FILE+ ;] TR-UNDER-SOURCE-FILES ;

: TR-UNDER-LINUX-KEY ( -- )
   s" target:linux-aarch64" CK-TEXT+
   s" src/os/linux/target.f" TR-KEY-FILE+
   s" src/os/linux/layout.f" TR-KEY-FILE+
   s" src/os/linux/sys.f" TR-KEY-FILE+
   s" src/os/linux/elf.f" TR-KEY-FILE+
   s" src/os/linux/sign.f" TR-KEY-FILE+
   s" src/os/linux/repl-term.f" TR-KEY-FILE+ ;

: TR-UNDER-MACOS-KEY ( -- )
   s" target:macos-aarch64" CK-TEXT+
   s" src/os/macos/target.f" TR-KEY-FILE+
   s" src/os/macos/layout.f" TR-KEY-FILE+
   s" src/os/macos/sys.f" TR-KEY-FILE+
   s" src/os/macos/macho.f" TR-KEY-FILE+
   s" src/os/macos/sign2.f" TR-KEY-FILE+
   s" src/os/macos/repl-term.f" TR-KEY-FILE+ ;

: TR-UNDER-TARGET-KEY ( -- )
   HB-TARGET-LINUX? if TR-UNDER-LINUX-KEY exit then
   HB-TARGET-MACOS? if TR-UNDER-MACOS-KEY exit then
   s" Habu-under-test cache unknown target" TR-FAIL ;

: TR-UNDER-KEY! ( -- )
   CK-RESET
   s" hb-under-test-cache-v2" CK-TEXT+
   s" bin/hb" TR-KEY-FILE+
   TR-UNDER-SOURCE-KEY
   TR-UNDER-TARGET-KEY
   TR-UNDER-KEY-HEX CK-FINAL-HEX ;

: TR-UNDER-NAME! ( -- )
   s" hb-under-" {: p:ptr pu:n :}
   pu TR-UNDER-PREFIX-U <> if E-STR-BOUNDS throw then
   p TR-UNDER-NAME-BUF pu BYTE-COPY
   TR-UNDER-KEY-HEX TR-UNDER-NAME-BUF pu + 64 BYTE-COPY
   pu 64 + TR-UNDER-NAME-U ! ;

: TR-UNDER-CACHE-PATHS ( -- )
   TR-UNDER-NAME!
   TR-PERSIST$ MAKE-DIRS
   TR-PERSIST$ TR-UNDER-NAME$ TR-UNDER-CACHE-BUF JOIN-PATH TR-UNDER-CACHE-U !
   TR-UNDER-CACHE$ s" .tmp" TR-UNDER-CACHE-TMP-BUF TR-UNDER-CACHE-TMP-U TR-SUFFIX!
   TR-UNDER-CACHE$ s" .lock" TR-UNDER-CACHE-LOCK-BUF TR-UNDER-CACHE-LOCK-U TR-SUFFIX! ;

: TR-UNDER-CACHE-KEY! ( -- )
   TR-UNDER-KEY!
   TR-UNDER-CACHE-PATHS ;

: TR-UNDER-CACHE-LOCK? ( -- bool )
   TR-UNDER-CACHE-LOCK$ FS-PATHZ FS-MUT-MODE-PRIVATE-DIR mkdir 0= if TR-TRUE exit then
   TR-UNDER-CACHE-LOCK$ DIR? if TR-FALSE exit then
   E-FS-IO throw ;

: TR-UNDER-CACHE-UNLOCK ( -- )
   TR-UNDER-CACHE-LOCK$ DIR? if TR-UNDER-CACHE-LOCK$ REMOVE-DIR then ;

: TR-UNDER-CACHE-RESTORE ( -- )
   TR-PERSIST? 0= if exit then
   TR-UNDER-CACHE-KEY!
   TR-UNDER-CACHE$ EXECUTABLE? 0= if s" candidate-cache-miss" GS-EVENT exit then
   s" candidate-cache-hit" GS-EVENT
   TR-UNDER-CACHE$ TR-UNDER$ COPY-FILE-STREAM
   TR-UNDER$ CHMOD-X
   -1 TR-UNDER-CACHE-HIT !
   -1 TR-UNDER-READY ! ;

: TR-UNDER-CACHE-INSTALL-LOCKED ( -- )
   TR-UNDER-CACHE$ EXECUTABLE? if exit then
   TR-UNDER-CACHE-TMP$ EXISTS? if TR-UNDER-CACHE-TMP$ REMOVE-FILE then
   TR-UNDER$ TR-UNDER-CACHE-TMP$ COPY-FILE-STREAM
   TR-UNDER-CACHE-TMP$ CHMOD-X
   TR-UNDER-CACHE-TMP$ TR-UNDER-CACHE$ RENAME-FILE
   s" candidate-cache-install" GS-EVENT ;

: TR-UNDER-CACHE-INSTALL ( -- )
   TR-PERSIST? 0= if exit then
   TR-UNDER-CACHE-HIT @ 0 <> if exit then
   TR-UNDER-CACHE-KEY!
   TR-UNDER-CACHE$ EXECUTABLE? if exit then
   TR-UNDER-CACHE-LOCK? 0= if exit then
   [: TR-UNDER-CACHE-INSTALL-LOCKED ;] catch TR-UNDER-CACHE-RC !
   TR-UNDER-CACHE-UNLOCK
   TR-UNDER-CACHE-RC @ 0 <> if TR-UNDER-CACHE-RC @ throw then ;

: TR-RUNNER-CACHED? ( -- bool )
   TR-RUNNER$ EXECUTABLE? 0= if 0 0= 0= exit then
   TR-RUNNER-TRUST$ FILE? 0= if 0 0= 0= exit then
   TR-RUNNER-STAMP$ FILE? 0= if 0 0= 0= exit then
   TR-RUNNER-STAMP$ TR-RUNNER-STAMP-RD 80 READ-ALL
   dup 64 <> if drop 0 0= 0= exit then
   TR-RUNNER-STAMP-RD swap TR-RUNNER-KEY-HEX 64 STR= ;

: TR-AOT-RUNNER-CACHED? ( -- bool )
   TR-AOT-RUNNER$ EXECUTABLE? 0= if 0 0= 0= exit then
   TR-AOT-RUNNER-TRUST$ FILE? 0= if 0 0= 0= exit then
   TR-AOT-RUNNER-STAMP$ FILE? 0= if 0 0= 0= exit then
   TR-AOT-RUNNER-STAMP$ TR-AOT-RUNNER-STAMP-RD 80 READ-ALL
   dup 64 <> if drop 0 0= 0= exit then
   TR-AOT-RUNNER-STAMP-RD swap TR-AOT-RUNNER-KEY-HEX 64 STR= ;

: TR-RUNNER-TOOL-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" TR-ARG+
   s" lib/errors.f" TR-ARG+
   s" lib/string.f" TR-ARG+
   s" lib/memory.f" TR-ARG+
   s" lib/fs.f" TR-ARG+
   s" lib/fs-mutate.f" TR-ARG+
   s" lib/process.f" TR-ARG+
   s" lib/process-argv.f" TR-ARG+
   s" lib/process-env.f" TR-ARG+
   s" lib/source.f" TR-ARG+
   s" lib/codesign.f" TR-ARG+
   s" tools/warm-image-lib.f" TR-ARG+
   s" tools/warm-image.f" TR-ARG+
   s" --" TR-ARG+
   TR-RUNNER$ TR-ARG+ ;

: TR-RUNNER-SUPPORT-ARGV ( -- )
   [: TR-ARG+ ;] TR-RUNNER-SUPPORT-FILES ;

: TR-AOT-RUNNER-TOOL-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" TR-ARG+
   s" lib/errors.f" TR-ARG+
   s" lib/string.f" TR-ARG+
   s" lib/memory.f" TR-ARG+
   s" lib/fs.f" TR-ARG+
   s" lib/fs-mutate.f" TR-ARG+
   s" lib/process.f" TR-ARG+
   s" lib/process-argv.f" TR-ARG+
   s" lib/process-env.f" TR-ARG+
   s" lib/source.f" TR-ARG+
   s" lib/codesign.f" TR-ARG+
   s" tools/warm-image-lib.f" TR-ARG+
   s" tools/warm-image.f" TR-ARG+
   s" --" TR-ARG+
   TR-AOT-RUNNER$ TR-ARG+ ;

: TR-AOT-RUNNER-SUPPORT-ARGV ( -- )
   [: TR-ARG+ ;] TR-AOT-RUNNER-SUPPORT-FILES ;

: TR-RUNNER-START ( -- )
   TR-RUNNER-PATHS
   TR-RUNNER-KEY!
   TR-RUNNER-CACHED? if s" warm-cache-hit" GS-EVENT -1 TR-RUNNER-READY ! exit then
   s" warm-cache-miss" GS-EVENT
   s" warm-build" GS-EVENT
   s" gate-runner-build" GS-EVENT
   TR-RUNNER-TOOL-ARGV
   TR-RUNNER-SUPPORT-ARGV
   PROC-ENV-RESET
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   TR-BUILD-CACHE-ENV
   GS-ENV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" s" native warm gate runner image" TR-TIMEOUT-MS
   TR-RUNNER-WARM-SLOT >IDX GT-POOL-START-SLOT ;

: TR-AOT-RUNNER-START ( -- )
   TR-AOT-RUNNER-PATHS
   TR-AOT-RUNNER-KEY!
   TR-AOT-RUNNER-CACHED? if s" warm-cache-hit" GS-EVENT -1 TR-AOT-RUNNER-READY ! exit then
   s" warm-cache-miss" GS-EVENT
   s" warm-build" GS-EVENT
   s" gate-runner-build" GS-EVENT
   TR-AOT-RUNNER-TOOL-ARGV
   TR-AOT-RUNNER-SUPPORT-ARGV
   PROC-ENV-RESET
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   TR-BUILD-CACHE-ENV
   GS-ENV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" s" native warm AOT gate runner image" TR-TIMEOUT-MS
   TR-AOT-RUNNER-SLOT >IDX GT-POOL-START-SLOT ;

: TR-RUNNER-EXPECT ( -- )
   TR-RUNNER$ EXECUTABLE? 0= if
      s" missing warm gate runner image" TR-FAIL
   then
   TR-RUNNER-TRUST$ FILE? 0= if
      s" missing warm gate runner trust file" TR-FAIL
   then
   TR-RUNNER-STAMP$ TR-RUNNER-KEY-HEX 64 WRITE-ALL
   -1 TR-RUNNER-READY ! ;

: TR-AOT-RUNNER-EXPECT ( -- )
   TR-AOT-RUNNER$ EXECUTABLE? 0= if
      s" missing warm AOT gate runner image" TR-FAIL
   then
   TR-AOT-RUNNER-TRUST$ FILE? 0= if
      s" missing warm AOT gate runner trust file" TR-FAIL
   then
   TR-AOT-RUNNER-STAMP$ TR-AOT-RUNNER-KEY-HEX 64 WRITE-ALL
   -1 TR-AOT-RUNNER-READY ! ;

: TR-RUNNER-DONE? ( -- bool )
   TR-RUNNER-READY @ 0 <> if 0 0= exit then
   TR-RUNNER-WARM-SLOT >IDX GT-POOL-DONE@ 0= if 0 0= 0= exit then
   TR-RUNNER-EXPECT
   0 0= ;

: TR-AOT-RUNNER-DONE? ( -- bool )
   TR-AOT-RUNNER-READY @ 0 <> if 0 0= exit then
   TR-AOT-RUNNER-SLOT >IDX GT-POOL-DONE@ 0= if 0 0= 0= exit then
   TR-AOT-RUNNER-EXPECT
   0 0= ;

: TR-DRAIN-UNTIL-AOT-RUNNER ( -- )
   begin TR-AOT-RUNNER-DONE? 0= while
      GT-POOL-STEP
   repeat ;

: TR-BUILD-COMMON ( -- )
   TR-COMMON
   TR-BUILD-ASSERT-LIBS
   s" test/gate-build-common.f"  >LEN PROC-ARGV+ ;

: TR-BUILD-LIB ( -- )
   s" lib/vector.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   s" lib/build.f"  >LEN PROC-ARGV+
   s" lib/codesign.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+
   s" tools/lint/token.f"  >LEN PROC-ARGV+
   s" tools/lint/lib.f"  >LEN PROC-ARGV+
   s" tools/lint/json-writer.f"  >LEN PROC-ARGV+
   s" tools/lint/source-lex.f"  >LEN PROC-ARGV+
   s" tools/aot-lint-core.f"  >LEN PROC-ARGV+
   s" tools/signature-lint-core.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" tools/warm-run.f"  >LEN PROC-ARGV+
   s" tools/hb-build-lib.f"  >LEN PROC-ARGV+
   s" tools/hb-build-direct-lints.f"  >LEN PROC-ARGV+ ;

: TR-BUILD-LIB-COMMON ( -- )
   TR-COMMON
   TR-BUILD-LIB
   TR-BUILD-ASSERT-LIBS
   s" test/gate-build-common.f"  >LEN PROC-ARGV+
   s" test/gate-build-hbb.f"  >LEN PROC-ARGV+ ;

: TR-STDLIB-ARGS ( -- )
   s" test/gate-pool.f"  >LEN PROC-ARGV+
   s" test/gate-stdlib.f"  >LEN PROC-ARGV+ ;

: TR-STDLIB-SLICE-ARGS ( ptr u8 n -- ) {: slice:ptr sliceu:n :}
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
   s" test/gate-pool.f"  >LEN PROC-ARGV+
   s" test/gate-engine.f"  >LEN PROC-ARGV+ ;

: TR-ENGINE-SLICE-ARGS ( ptr u8 n -- ) {: slice:ptr sliceu:n :}
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

: TR-DIAG-SLICE-ARGS ( ptr u8 n -- ) {: slice:ptr sliceu:n :}
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
   TR-BUILD-LIB-COMMON
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

: TR-PHASE-LABEL ( idx -- ptr u8 n ) {: idx:idx :}
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

: TR-PHASE-DIR ( idx -- ptr u8 n ) {: idx:idx :}
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

: TR-PHASE-ARGS ( idx -- ) {: idx:idx :}
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

: TR-PHASE-RUNNER-TOKEN ( idx -- ptr u8 n ) {: idx:idx :}
   idx IDX>N 2 = if s" tool" exit then
   idx IDX>N 3 = if s" check-cli" exit then
   idx IDX>N 4 = if s" tail" exit then
   idx IDX>N 5 = if s" repair" exit then
   idx IDX>N 6 = if s" debug" exit then
   idx IDX>N 7 = if s" aot-pos" exit then
   idx IDX>N 8 = if s" aot-neg" exit then
   idx IDX>N 9 = if s" fixtures" exit then
   idx IDX>N 10 = if s" diag-repair" exit then
   idx IDX>N 11 = if s" diag-undef-primary" exit then
   idx IDX>N 12 = if s" diag-all-strict" exit then
   idx IDX>N 13 = if s" diag-file-unsafe" exit then
   idx IDX>N 14 = if s" dictionary" exit then
   idx IDX>N 16 = if s" runtime" exit then
   idx IDX>N 17 = if s" lint-tools" exit then
   idx IDX>N 18 = if s" lint-manifest" exit then
   idx IDX>N 19 = if s" lint-artifacts" exit then
   idx IDX>N 20 = if s" lint-libs" exit then
   E-TBL-BOUNDS throw ;

: TR-PHASE-TMP! ( idx -- ) {: idx:idx :}
   GT-ROOT idx TR-PHASE-DIR TR-PATH-BUF JOIN-PATH TR-PATH-U !
   TR-PATH$ MAKE-DIRS ;

: TR-STDLIB-SLICE? ( idx -- bool ) {: idx:idx :}
   idx IDX>N 2 >= idx IDX>N 4 <= and
   idx IDX>N 17 = or
   idx IDX>N 18 = or
   idx IDX>N 19 = or
   idx IDX>N 20 = or ;

: TR-TOOLS-PHASE? ( idx -- bool ) {: idx:idx :}
   idx TR-STDLIB-SLICE? if 0 0= exit then
   idx IDX>N 5 = ;

: TR-NESTED-POOL-SLOTS$ ( -- ptr u8 n )
   s" 4" ;

: TR-PHASE-POOL-ENV ( idx -- ) {: idx:idx :}
   idx TR-STDLIB-SLICE? if
      s" HABU_GATE_POOL_SLOTS" >LEN TR-NESTED-POOL-SLOTS$ >LEN PROC-ENV+
      exit
   then
   idx IDX>N 9 = if
      s" HABU_GATE_POOL_SLOTS" >LEN TR-NESTED-POOL-SLOTS$ >LEN PROC-ENV+
   then ;

: TR-PHASE-TOOLS-ENV ( idx -- ) {: idx:idx :}
   idx TR-TOOLS-PHASE? if TR-TOOLS-ENV then
   idx IDX>N 3 = if TR-CHECK-ENV then ;

: TR-PHASE-UNDER-ENV? ( idx -- bool ) {: idx:idx :}
   idx IDX>N 15 = if 0 0= exit then
   TR-UNDER-READY @ 0 <> ;

: TR-PHASE-UNDER-EXE? ( idx -- bool ) {: idx:idx :}
   idx IDX>N 15 = if 0 0= 0= exit then
   TR-UNDER-READY @ 0 <> ;

: TR-PHASE-RUNNER? ( idx -- bool ) {: idx:idx :}
   TR-RUNNER-READY @ 0= if 0 0= 0= exit then
   idx IDX>N 0= if 0 0= 0= exit then
   idx IDX>N 1 = if 0 0= 0= exit then
   idx IDX>N 7 = if 0 0= 0= exit then
   idx IDX>N 8 = if 0 0= 0= exit then
   idx IDX>N 15 = if 0 0= 0= exit then
   0 0= ;

: TR-PHASE-AOT-RUNNER? ( idx -- bool ) {: idx:idx :}
   TR-AOT-RUNNER-READY @ 0= if 0 0= 0= exit then
   idx IDX>N 7 = if 0 0= exit then
   idx IDX>N 8 = ;

: TR-PHASE-WARM-RUNNER? ( idx -- bool ) {: idx:idx :}
   idx TR-PHASE-AOT-RUNNER? if 0 0= exit then
   idx TR-PHASE-RUNNER? ;

: TR-PHASE-UNDER-ENV ( idx -- ) {: idx:idx :}
   idx TR-PHASE-UNDER-ENV? if
      s" under-env" GS-EVENT
      TR-UNDER-ENV+
   then ;

: TR-PHASE-EXE ( idx -- ptr u8 n ) {: idx:idx :}
   idx TR-PHASE-AOT-RUNNER? if TR-AOT-RUNNER$ exit then
   idx TR-PHASE-RUNNER? if TR-RUNNER$ exit then
   idx TR-PHASE-UNDER-EXE? if TR-UNDER$ exit then
   s" bin/hb" ;

: TR-PHASE-ARGV-COLD ( -- )
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

: TR-PHASE-ARGV-RUNNER ( idx -- ) {: idx:idx :}
   s" --load"  >LEN PROC-ARGV+
   s" test/gate-runner-entry.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   idx TR-PHASE-RUNNER-TOKEN >LEN PROC-ARGV+ ;

: TR-PHASE-ARGV-AOT-RUNNER ( idx -- ) {: idx:idx :}
   s" --load"  >LEN PROC-ARGV+
   s" test/gate-aot-runner-entry.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   idx TR-PHASE-RUNNER-TOKEN >LEN PROC-ARGV+ ;

: TR-PHASE-BASE ( idx -- ) {: idx:idx :}
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
   idx TR-PHASE-AOT-RUNNER? if
      idx TR-PHASE-ARGV-AOT-RUNNER
   else idx TR-PHASE-RUNNER? if
      idx TR-PHASE-ARGV-RUNNER
   else
      TR-PHASE-ARGV-COLD
   then then ;

: TR-PHASE-START ( idx -- ) {: idx:idx :}
   idx TR-PHASE-BASE
   idx TR-PHASE-WARM-RUNNER? 0= if idx TR-PHASE-ARGS then
   s" top-phase-spawn" GS-EVENT
   idx TR-PHASE-AOT-RUNNER? if s" runner-phase-spawn" GS-EVENT then
   idx TR-PHASE-RUNNER? if s" runner-phase-spawn" GS-EVENT then
   idx TR-PHASE-UNDER-EXE? if s" under-phase-spawn" GS-EVENT then
   idx TR-PHASE-EXE idx TR-PHASE-LABEL TR-TIMEOUT-MS GT-POOL-START ;

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
   TR-EXPECT-UNDER
   TR-UNDER-CACHE-INSTALL ;

: TR-DRAIN-UNTIL-WARM ( -- )
   begin TR-WARM-DONE? 0= while
      GT-POOL-STEP
   repeat ;

: TR-DRAIN-UNTIL-CHECK-WARM ( -- )
   begin TR-CHECK-WARM-DONE? 0= while
      GT-POOL-STEP
   repeat ;

: TR-DRAIN-UNTIL-RUNNER ( -- )
   begin TR-RUNNER-DONE? 0= while
      GT-POOL-STEP
   repeat ;

: TR-CHECK-WARM-ORDER@ ( idx -- idx ) {: idx:idx :}
   idx IDX>N cells TR-CHECK-WARM-ORDER + @ >IDX ;

: TR-LATE-ORDER@ ( idx -- idx ) {: idx:idx :}
   idx IDX>N cells TR-LATE-ORDER + @ >IDX ;

: TR-MANIFEST-EARLY? ( -- bool )
   TR-MANIFEST-EARLY @ 0 <> ;

: TR-LIBS-EARLY? ( -- bool )
   TR-LIBS-EARLY @ 0 <> ;

: TR-TRY-EARLY-LINTS ( -- )
   TR-UNDER-READY @ 0= if exit then
   18 >IDX TR-PHASE-START
   -1 TR-MANIFEST-EARLY !
   20 >IDX TR-PHASE-START
   -1 TR-LIBS-EARLY ! ;

: TR-LATE-SKIP? ( idx -- bool ) {: idx:idx :}
   idx IDX>N 18 = TR-MANIFEST-EARLY? and
   idx IDX>N 20 = TR-LIBS-EARLY? and or ;

: TR-EARLY-START ( -- )
   GT-POOL-RESET
   TR-WARM-READY-RESET
   0 >IDX TR-PHASE-START
   1 >IDX TR-PHASE-START
   TR-RUNNER-START
   TR-AOT-RUNNER-START
   TR-UNDER-READY @ 0= if 15 >IDX TR-PHASE-START then
   TR-DRAIN-UNTIL-RUNNER
   6 >IDX TR-PHASE-START
   TR-TRY-EARLY-LINTS ;

: TR-LATE-START ( -- )
   0 begin dup TR-LATE-PHASES < while
      dup >IDX TR-LATE-ORDER@
      dup TR-LATE-SKIP? if drop else TR-PHASE-START then
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
   TR-DRAIN-UNTIL-AOT-RUNNER
   TR-WORK-DRAIN ;

: TR-MAIN ( -- )
   TR-GATE-START!
   TR-CHECK-ARGS
   TR-START
   TR-CLEAN-WARM
   TR-EXPECT-HB
   TR-UNDER-CACHE-RESTORE
   TR-DAG-RUN
   GS-SUMMARY
   GT-CLEANUP
   TR-FINISH ;

TR-MAIN
