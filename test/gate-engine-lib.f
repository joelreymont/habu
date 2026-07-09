\ gate-engine.f - checked runner for engine and public hb gate checks.
\
\ Load after test/gate-common.f, lib/memory.f, lib/build.f, lib/codesign.f,
\ and tools/build-fixpoint.f.

require test/gate-build-size.f

64 constant GENG-USAGE-RC
67 constant GE-UNCAUGHT-RC       \ deterministic exit status for an uncaught top-level throw
0 constant GENG-ALL-ID
1 constant GENG-BUILD-ID
2 constant GENG-FIXTURES-ID
3 constant GENG-REPAIR-ID
4 constant GENG-RUNTIME-ID
5 constant GENG-VALIDATE-ID
$40000 constant GE-MAX-CANDIDATE-BYTES

create GE-SCRIPT-PATH FS-PATH-CAP allot
create GE-CAND-PATH FS-PATH-CAP allot
create GE-SRC-CAND-PATH FS-PATH-CAP allot

variable GE-SCRIPT-U
variable GE-CAND-U
variable GE-SRC-CAND-U
variable GENG-SLICE
variable GE-PROF-I
variable GE-REG-I
variable GE-JIT-I
variable GE-IMG-I
variable GE-IMG-BUILD-I
variable GE-HABU1-I
variable GENG-ARG-I
variable GENG-SLICE-SEEN
variable GE-BAD-TYPED-VERDICT

create GE-CHECK-OFF-LINE
10 c, 48 c, 32 c, 115 c, 101 c, 116 c, 45 c,
99 c, 104 c, 101 c, 99 c, 107 c, 10 c,

: GENG-USAGE ( -- )
   s" usage: test/gate-engine.f [build|fixtures|repair|runtime|validate] [--pool-slots N]" GENG-USAGE-RC die ;

: GENG-ARG$ ( -- ptr u8 n )
   GENG-ARG-I @ SCRIPT-ARGV$ ;

: GENG-ARG-VALUE$ ( -- ptr u8 n )
   GENG-ARG-I @ 1+ SCRIPT-ARGC >= if GENG-USAGE then
   GENG-ARG-I @ 1+ SCRIPT-ARGV$ ;

: GENG-POS-NUM ( ptr u8 n -- n )
   STR>NUMBER? 0= if drop GENG-USAGE then
   dup 1 < if drop GENG-USAGE then ;

: GENG-ADVANCE ( n -- )
   GENG-ARG-I @ + GENG-ARG-I ! ;

: GENG-SLICE! ( n -- )
   GENG-SLICE ! ;

: GENG-POOL-OPT ( -- )
   GENG-ARG-VALUE$ GENG-POS-NUM GT-POOL-SLOTS!
   2 GENG-ADVANCE ;

: GENG-SLICE-ARG? ( -- bool )
   GENG-ARG$ s" build" STR= if GENG-BUILD-ID GENG-SLICE! 0 0= exit then
   GENG-ARG$ s" fixtures" STR= if GENG-FIXTURES-ID GENG-SLICE! 0 0= exit then
   GENG-ARG$ s" repair" STR= if GENG-REPAIR-ID GENG-SLICE! 0 0= exit then
   GENG-ARG$ s" runtime" STR= if GENG-RUNTIME-ID GENG-SLICE! 0 0= exit then
   GENG-ARG$ s" validate" STR= if GENG-VALIDATE-ID GENG-SLICE! 0 0= exit then
   0 0= 0= ;

: GENG-SLICE-OPT ( -- )
   GENG-SLICE-SEEN @ if GENG-USAGE then
   GENG-SLICE-ARG? 0= if GENG-USAGE then
   -1 GENG-SLICE-SEEN !
   1 GENG-ADVANCE ;

: GENG-PARSE-ARG ( -- )
   GENG-ARG$ s" --pool-slots" STR= if GENG-POOL-OPT exit then
   GENG-SLICE-OPT ;

: GENG-PARSE-SLICE ( -- )
   GENG-ALL-ID GENG-SLICE!
   0 GENG-SLICE-SEEN !
   0 GENG-ARG-I !
   begin GENG-ARG-I @ SCRIPT-ARGC < while
      GENG-PARSE-ARG
   repeat ;

GE-FILES: GE-ENGINE-STDLIB-CHECK-FILES
   lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f lib/process-env.f lib/process-cwd.f
;GE-FILES

GE-FILES: GE-REPAIR-HINTS-RUN-FILES
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/vector.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f tools/lint/text.f tools/lint/token.f
   tools/lint/lib.f tools/lint/json-writer.f tools/lint/source-lex.f
   tools/check-all-errors-core.f tools/cli-run.f
   tools/json.f tools/gate-json-assert-core.f tools/check-repair-hints-test.f
;GE-FILES

: GE-LOAD-RESET ( -- )
   GE-HB-RESET
   s" --load" GE-ARG+ ;

: GE-ENGINE-STDLIB-CHECK ( -- )
   GE-SRC-RESET
   [: GE-SRC-FILE+ ;] GE-ENGINE-STDLIB-CHECK-FILES
   s" engine stdlib support check" GE-CHECK-SRC-LIST ;

: GE-REPAIR-HINTS-RUN ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-REPAIR-HINTS-RUN-FILES
   s" repair diagnostic hints" GE-HB-RUN ;

: GE-RUN-ENV-ASYNC ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n timeout:n label:ptr labelu:n :}
   label labelu GS-HELPER-EVENT
   PROC-ENV-INHERIT-MISSING
   path pathu label labelu timeout GT-POOL-START ;

: GE-HB-RUN-ASYNC ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GS-INNER-HB-EVENT
   label labelu GS-BOUNDARY-EVENT
   GE-HB$ GE-TIMEOUT-MS label labelu GE-RUN-ENV-ASYNC ;

: GE-BIN-HB-RUN-ASYNC ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GS-INNER-HB-EVENT
   label labelu GS-BOUNDARY-EVENT
   s" bin/hb" GE-TIMEOUT-MS label labelu GE-RUN-ENV-ASYNC ;

: GE-FIXTURE-INCLUDE ( ptr u8 n -- )
   s" inprocess-eval" GS-EVENT
   included ;

: GE-FS-MUTATE-FIXTURE ( -- )
   s" lib/fs-mutate-test.f" GE-FIXTURE-INCLUDE ;

: GE-PROCESS-ARGV-FIXTURE ( -- )
   s" lib/process-argv-test.f" GE-FIXTURE-INCLUDE ;

: GE-PROCESS-ENV-FIXTURE ( -- )
   s" lib/process-env-test.f" GE-FIXTURE-INCLUDE ;

: GE-PROCESS-CWD-FIXTURE ( -- )
   s" lib/process-cwd-test.f" GE-FIXTURE-INCLUDE ;

: GE-HB-BASELINE-FIXTURE ( -- )
   s" tools/hb-baseline-contracts-test.f" GE-FIXTURE-INCLUDE ;

: GE-FS-MUTATE-FIXTURE-ASYNC ( -- )
   s" fs mutation stdlib" GE-TIMEOUT-MS [: GE-FS-MUTATE-FIXTURE ;] GT-POOL-START-FORK ;

: GE-PROCESS-ARGV-FIXTURE-ASYNC ( -- )
   s" process argv stdlib" GE-TIMEOUT-MS [: GE-PROCESS-ARGV-FIXTURE ;] GT-POOL-START-FORK ;

: GE-PROCESS-ENV-FIXTURE-ASYNC ( -- )
   s" process env stdlib" GE-TIMEOUT-MS [: GE-PROCESS-ENV-FIXTURE ;] GT-POOL-START-FORK ;

: GE-PROCESS-CWD-FIXTURE-ASYNC ( -- )
   s" process cwd stdlib" GE-TIMEOUT-MS [: GE-PROCESS-CWD-FIXTURE ;] GT-POOL-START-FORK ;

: GE-HB-BASELINE-FIXTURE-ASYNC ( -- )
   s" hb baseline contracts" GE-TIMEOUT-MS [: GE-HB-BASELINE-FIXTURE ;] GT-POOL-START-FORK ;

: GE-CANDIDATE-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a GE-CAND-PATH u BYTE-COPY
   u GE-CAND-U ! ;

: GE-SRC-CANDIDATE-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a GE-SRC-CAND-PATH u BYTE-COPY
   u GE-SRC-CAND-U ! ;

: GE-DEFAULT-CANDIDATE! ( -- )
   GT-ROOT s" hb-stdin" GE-CAND-PATH JOIN-PATH GE-CAND-U ! ;

: GE-ENV-CANDIDATE? ( -- bool )
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop 0 0= 0= exit
   then
   GE-CANDIDATE-PATH!
   0 0= ;

: GE-CANDIDATE-SET? ( -- bool )
   GE-CAND-U @ 0 > ;

: GE-CANDIDATE! ( -- )
   GE-CANDIDATE-SET? if exit then
   GE-ENV-CANDIDATE? if exit then
   GE-DEFAULT-CANDIDATE! ;

: GE-CANDIDATE$ ( -- ptr u8 n )
   GE-CAND-PATH GE-CAND-U @ ;

: GE-SRC-CANDIDATE$ ( -- ptr u8 n )
   GE-SRC-CAND-PATH GE-SRC-CAND-U @ ;

: GE-EXPECT-CANDIDATE ( -- )
   GE-CANDIDATE$ EXECUTABLE? 0= if
      s" Habu-under-test candidate executable" GE-FAIL
   then ;

: GE-SRC-CANDIDATE! ( -- )
   s" hb-stdin" BF-A$ GE-SRC-CANDIDATE-PATH! ;

: GE-CANDIDATE-SIZE-CHECK ( -- )
   GE-CANDIDATE$ FILE-SIZE GE-MAX-CANDIDATE-BYTES > if
      s" Habu-under-test candidate too large" GE-FAIL
   then
   GE-CANDIDATE$ GB-SIZE-RATCHET ;

: GE-REMOVE-CANDIDATE ( -- )
   GE-CANDIDATE$ EXISTS? if GE-CANDIDATE$ REMOVE-FILE then ;

: GE-PROMOTE-CANDIDATE ( -- )
   GE-CANDIDATE!
   GE-SRC-CANDIDATE!
   GE-SRC-CANDIDATE$ GE-CANDIDATE$ STR= if exit then
   GE-REMOVE-CANDIDATE
   GE-SRC-CANDIDATE$ GE-CANDIDATE$ RENAME-FILE
   GE-CANDIDATE$ CHMOD-X ;

: GE-CHECK-OFF-LINE$ ( -- ptr u8 n )
   GE-CHECK-OFF-LINE 13 ;

: GE-OLD-HOOK$ ( -- ptr u8 n )
   SB-RESET
   s" : HOOK ( ptr u8 n -- n ) CHECK! " SB-APPEND
   s" dup -1 <> if 70 throw then ; ' HOOK set-check" SB-APPEND
   SB$ ;

: GE-STAGE2-HOOK$ ( -- ptr u8 n )
   s" ' HOOK set-check" ;

: GE-READ-BUILD-TMP ( ptr u8 n -- ptr u8 n ) {: name:ptr nameu:n :}
   name nameu BF-A$ FILE-SIZE MEM-ALLOC-64K-SPAN {: buf:ptr cap:n :}
   name nameu BF-A$ buf cap READ-ALL {: got:n :}
   buf got ;

: GE-SHAPE-HAS ( ptr u8 n ptr u8 n ptr u8 n -- ) {: a:ptr u:n needle:ptr needleu:n label:ptr labelu:n :}
   a u needle needleu CONTAINS? 0= if label labelu GE-FAIL then ;

: GE-SHAPE-LACKS ( ptr u8 n ptr u8 n ptr u8 n -- ) {: a:ptr u:n needle:ptr needleu:n label:ptr labelu:n :}
   a u needle needleu CONTAINS? if label labelu GE-FAIL then ;

: GE-SHAPE-FIND ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n needle:ptr needleu:n :}
   a u needle needleu FIND-SUB ;

: GE-SHAPE-FIND-AFTER ( ptr u8 n n ptr u8 n -- n ) {: a:ptr u:n start:n needle:ptr needleu:n :}
   start 0 < if -1 exit then
   start u >= if -1 exit then
   a start BYTE+ u start - needle needleu FIND-SUB
   dup 0 < if exit then
   start + ;

: GE-SHAPE-FOUND ( n ptr u8 n -- n ) {: pos:n label:ptr labelu:n :}
   pos 0 < if label labelu GE-FAIL then
   pos ;

: GE-SHAPE-NOT-FOUND ( n ptr u8 n -- )
   {: pos:n label:ptr labelu:n :}
   pos 0 >= if label labelu GE-FAIL then ;

: GE-STAGE2-SOURCE-SHAPE ( -- )
   s" stage2-src" GE-READ-BUILD-TMP {: a:ptr u:n :}
   a u GE-OLD-HOOK$ s" build stage2 stale hook" GE-SHAPE-LACKS
   a u s" HB-CHECK-HOOK" s" build stage2 duplicate hook def" GE-SHAPE-LACKS
   a u s" 0 set-check" s" build stage2 unchecked boundary" GE-SHAPE-HAS
   a u GE-STAGE2-HOOK$ s" build stage2 hook install" GE-SHAPE-HAS
   a u s" STDIN-OUT" s" build stage2 stdin output" GE-SHAPE-HAS ;

: GE-STAGE2-SCRATCH-SHAPE ( -- )
   BF-STAGE2-SOURCE
   s" stage2-src" GE-READ-BUILD-TMP {: a:ptr u:n :}
   a u s" S2-SOURCE-CAP allot" s" build stage2 static source buffer" GE-SHAPE-LACKS
   a u s" stage2: source mmap failed" s" build stage2 mmap source" GE-SHAPE-HAS ;

: GE-STAGE2-ORDER-SHAPE ( -- )
   s" stage2-src" GE-READ-BUILD-TMP {: a:ptr u:n :}
   a u s" : BPROF-ON" GE-SHAPE-FIND s" build stage2 prof" GE-SHAPE-FOUND GE-PROF-I !
   a u GE-PROF-I @ s" : EMIT-VRINIT" GE-SHAPE-FIND-AFTER s" build stage2 regalloc" GE-SHAPE-FOUND GE-REG-I !
   a u GE-REG-I @ s" : FOLD-ENTRY" GE-SHAPE-FIND-AFTER s" build stage2 jit" GE-SHAPE-FOUND GE-JIT-I !
   GE-PROF-I @ GE-REG-I @ >= if s" build stage2 prof/reg order" GE-FAIL then
   GE-REG-I @ GE-JIT-I @ >= if s" build stage2 reg/jit order" GE-FAIL then ;

: GE-STAGE2-IMAGE-SHAPE ( -- )
   s" stage2-src" GE-READ-BUILD-TMP {: a:ptr u:n :}
   a u s" : ASM-CODELEN!" GE-SHAPE-FIND s" build stage2 image token" GE-SHAPE-FOUND GE-IMG-I !
   a u GE-IMG-I @ s" : BUILD-IMAGE" GE-SHAPE-FIND-AFTER s" build stage2 image build" GE-SHAPE-FOUND GE-IMG-BUILD-I !
   a u GE-IMG-BUILD-I @ s" : RPD@" GE-SHAPE-FIND-AFTER s" build stage2 habu1 after image" GE-SHAPE-FOUND GE-HABU1-I !
   a u GE-IMG-BUILD-I @ GE-CHECK-OFF-LINE$ GE-SHAPE-FIND-AFTER s" build stage2 image unchecked span" GE-SHAPE-NOT-FOUND
   GE-IMG-I @ GE-IMG-BUILD-I @ >= if s" build stage2 image order" GE-FAIL then
   GE-IMG-BUILD-I @ GE-HABU1-I @ >= if s" build stage2 habu1 order" GE-FAIL then ;

: GE-BUILD-SOURCE-SHAPE ( -- )
   GE-STAGE2-SOURCE-SHAPE
   GE-STAGE2-ORDER-SHAPE
   GE-STAGE2-IMAGE-SHAPE ;

: GE-BUILD-FIXPOINT ( -- )
   s" candidate-build" GS-EVENT
   s" hb-gate-engine" GT-START
   GT-ROOT BF-TMP!
   BF-PREFLIGHT
   BF-STAGE2-SOURCE
   GE-STAGE2-SCRATCH-SHAPE
   BF-STAGE-FIXPOINT-FROM-SOURCE
   BF-BUILD-STDIN-FROM-STAGE
   GE-BUILD-SOURCE-SHAPE
   GE-PROMOTE-CANDIDATE
   BF-TMP-RESET
   GE-EXPECT-CANDIDATE
   GE-CANDIDATE-SIZE-CHECK
   s" PASS: self-rebuild fixpoint" type cr ;

: GE-RUN-STD-FIXTURES ( -- )
   GT-POOL-RESET
   GE-FS-MUTATE-FIXTURE-ASYNC
   GE-PROCESS-ARGV-FIXTURE-ASYNC
   GE-PROCESS-ENV-FIXTURE-ASYNC
   GE-PROCESS-CWD-FIXTURE-ASYNC
   GE-HB-BASELINE-FIXTURE-ASYNC
   GE-ENGINE-STDLIB-CHECK
   GT-POOL-DRAIN ;

: GE-RUN-EXTRA-FIXTURES ( -- )
   GE-RUN-STD-FIXTURES
   GE-REPAIR-HINTS-RUN ;

: GE-SUITE-RUN ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n label:ptr labelu:n :}
   exe exeu GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-OK
   SB-RESET s" ok" SB-APPEND GE-SB-LF
   GT-OUT$ SB$ ENDS-WITH? 0= if label labelu GE-FAIL then
   s" PASS: " type label labelu type cr ;

: GE-ENGINE-SUITE-ON ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   s" test/engine-suite.f" GE-SRC-FILE+
   exe exeu label labelu GE-SUITE-RUN ;

: GE-TYPE-FAMILY-SUITE-ON ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   s" test/type-family-suite.f" GE-SRC-FILE+
   exe exeu label labelu GE-SUITE-RUN ;

: GE-ENGINE-SUITE ( -- )
   GE-CANDIDATE$ s" engine suite on Habu-under-test" GE-ENGINE-SUITE-ON
   s" bin/hb" s" engine suite on bin/hb" GE-ENGINE-SUITE-ON ;

: GE-TYPE-FAMILY-SUITE ( -- )
   GE-CANDIDATE$ s" type-family suite on Habu-under-test" GE-TYPE-FAMILY-SUITE-ON
   s" bin/hb" s" type-family suite on bin/hb" GE-TYPE-FAMILY-SUITE-ON ;

: GE-TYPE-DECL-SUITE-ON ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   s" test/type-decl-suite.f" GE-SRC-FILE+
   exe exeu label labelu GE-SUITE-RUN ;

: GE-TYPE-DECL-SUITE ( -- )
   GE-CANDIDATE$ s" type-decl suite on Habu-under-test" GE-TYPE-DECL-SUITE-ON
   s" bin/hb" s" type-decl suite on bin/hb" GE-TYPE-DECL-SUITE-ON ;

: GE-TYPE-CTOR-SUITE-ON ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   s" test/type-ctor-suite.f" GE-SRC-FILE+
   exe exeu label labelu GE-SUITE-RUN ;

: GE-TYPE-CTOR-SUITE ( -- )
   GE-CANDIDATE$ s" type-ctor suite on Habu-under-test" GE-TYPE-CTOR-SUITE-ON
   s" bin/hb" s" type-ctor suite on bin/hb" GE-TYPE-CTOR-SUITE-ON ;

: GE-TYPE-LINEAR-SUITE-ON ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   s" test/type-linear-suite.f" GE-SRC-FILE+
   exe exeu label labelu GE-SUITE-RUN ;

: GE-TYPE-LINEAR-SUITE ( -- )
   GE-CANDIDATE$ s" type-linear suite on Habu-under-test" GE-TYPE-LINEAR-SUITE-ON
   s" bin/hb" s" type-linear suite on bin/hb" GE-TYPE-LINEAR-SUITE-ON ;

: GE-TYPE-MATCH-SUITE-ON ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   s" test/type-match-suite.f" GE-SRC-FILE+
   exe exeu label labelu GE-SUITE-RUN ;

\ TFAM 9 slice 3 (Gate 17j): checked MATCH eliminator — exhaustiveness,
\ payload refinement, joins, linear consumption, depth fail-closure.
: GE-TYPE-MATCH-SUITE ( -- )
   GE-CANDIDATE$ s" type-match suite on Habu-under-test" GE-TYPE-MATCH-SUITE-ON
   s" bin/hb" s" type-match suite on bin/hb" GE-TYPE-MATCH-SUITE-ON ;

: GE-TYPE-LAYOUT-SUITE-ON ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   s" test/type-layout-lower-pending.f" GE-SRC-FILE+
   exe exeu label labelu GE-SUITE-RUN ;

\ TFAM 12 slice 3b: width-fact contracts, pass-2 lowering goldens, and
\ whole-bundle transport execution rows (see the suite file's header).
: GE-TYPE-LAYOUT-SUITE ( -- )
   GE-CANDIDATE$ s" type-layout suite on Habu-under-test" GE-TYPE-LAYOUT-SUITE-ON
   s" bin/hb" s" type-layout suite on bin/hb" GE-TYPE-LAYOUT-SUITE-ON ;

\ The former GE-CAND-SMOKE (hook-installed / checked-compile-run / baked-word-
\ resolves) is now three T= probes inside test/engine-suite.f, so it rides the
\ existing engine-suite candidate launch (GE-ENGINE-SUITE-ON) instead of a second
\ HABU_UNDER_TEST spawn per candidate. See engine-suite.f "candidate ... smoke".

\ An uncaught top-level throw reaches the engine's BTHROW no-handler path
\ (habu1.f THROW-NOREC). Before the fix it exit_group'd the RAW code, so the
\ kernel masked it to 8 bits: -2816 (a multiple of 256) exited 0 SILENTLY and
\ -2802 exited 14 SILENTLY - fail-open for any tool reading the exit status.
\ Now a kernel-representable code in [1,255] still exits byte-identically to
\ before (deliberate exit contracts: argv usage 64, check hook 70, lint
\ findings 1), while any other code is named on fd 2 and exits GE-UNCAUGHT-RC.
: GE-UNCAUGHT-RUN ( ptr u8 n n ptr u8 n -- )
   {: src:ptr srcu:n want:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   src srcu GE-SRC-LINE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   want label labelu GE-EXPECT-RC ;

: GE-UNCAUGHT-CASE ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n want:n needle:ptr needleu:n label:ptr labelu:n :}
   src srcu want label labelu GE-UNCAUGHT-RUN
   needle needleu label labelu GE-EXPECT-ERR-HAS ;

: GE-UNCAUGHT-THROW ( -- )
   s" -2816 throw" GE-UNCAUGHT-RC s" uncaught throw code -2816"
      s" uncaught throw -2816 (kernel-masks-to-0)" GE-UNCAUGHT-CASE
   s" -2802 throw" GE-UNCAUGHT-RC s" uncaught throw code -2802"
      s" uncaught throw -2802 (kernel-masks-to-14)" GE-UNCAUGHT-CASE
   s" 70 throw" 70 s" uncaught throw 70 representable passthrough" GE-UNCAUGHT-RUN
   s" uncaught throw 70 representable passthrough" GE-EXPECT-SILENT
   s" : GEUT ( -- ) [: -2816 throw ;] catch . ;  GEUT" 0
      s" caught throw stays in-process rc 0" GE-UNCAUGHT-RUN
   SB-RESET s" -2816" SB-APPEND GE-SB-LF
   SB$ s" caught throw control output" GE-EXPECT-OUT
   s" PASS: uncaught top-level throw exits are reported, never masked" type cr ;

\ Interpret-mode transports of a wide layout bundle SILENTLY CORRUPTED: the
\ top-level stack ops move one physical cell, so a TRUSTED-seeded 2-cell
\ bundle followed by `dup . . . .` printed the tag twice and then read below
\ the seed (9 9 7 <garbage>, rc 0) - fail-open through any TRUSTED boundary
\ at the unchecked REPL (dot habu-tfam-12-interpret-10b385b1). The engine
\ fails closed: executing (or ticking) a DNAME-WIDE-flagged word at interpret
\ level dies with a named diagnostic before the bundle can land on the
\ untyped interpret stack. The flag is CHECKER-COMPUTED: the record choke
\ point (E-ADD-EFFECT) scans the four effect rows with T-WIDTH (quotation
\ sub-effects included) and the engine publish tails consume the latch
\ (rec-wide-publish -> wide-mark) after ndict++ — no manual marking anywhere
\ in this fixture. Checked definitions own bundle work; the guard leg proves
\ a compiled call of the SAME marked word still compiles and runs at top
\ level, and the scalar leg proves a one-cell TRUSTED word stays unmarked.
: GE-ILAYOUT-PRELUDE ( -- )
   s" SUMTYPE gewide 2" GE-SRC-LINE
   s"   VARIANT ok a ;VARIANT" GE-SRC-LINE
   s"   VARIANT err b ;VARIANT" GE-SRC-LINE
   s" ;SUMTYPE" GE-SRC-LINE
   s" TRUSTED: GE-WMK ( -- gewide<n,n> ) 7 9 ;" GE-SRC-LINE ;

: GE-ILAYOUT-CASE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   GE-ILAYOUT-PRELUDE
   src srcu GE-SRC-LINE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   70 label labelu GE-EXPECT-RC
   s" interpret-mode layout value" label labelu GE-EXPECT-ERR-HAS ;

: GE-ILAYOUT-GUARD ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   GE-ILAYOUT-PRELUDE
   s" TRUSTED: GE-WUN ( gewide<n,n> -- n n ) ;" GE-SRC-LINE
   s" : GE-WRUN ( -- n n ) GE-WMK GE-WUN ;" GE-SRC-LINE
   s" GE-WRUN . ." GE-SRC-LINE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" checked wide transport guard" GE-EXPECT-OK
   SB-RESET s" 9" SB-APPEND GE-SB-LF s" 7" SB-APPEND GE-SB-LF
   SB$ s" checked wide transport guard output" GE-EXPECT-OUT ;

\ negative control: a one-cell TRUSTED word is NOT marked by the checker scan
\ and still interprets at top level (rc 0, value printed).
: GE-ILAYOUT-SCALAR ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   GE-ILAYOUT-PRELUDE
   s" TRUSTED: GE-WN ( -- n ) 42 ;" GE-SRC-LINE
   s" GE-WN ." GE-SRC-LINE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" scalar trusted word interprets" GE-EXPECT-OK
   SB-RESET s" 42" SB-APPEND GE-SB-LF
   SB$ s" scalar trusted word output" GE-EXPECT-OUT ;

\ does>-split wide facts fail closed at the pass-2 trigger with a fixed
\ label (previously a lone current-token write - unattributable; TFAM 12
\ item 3 verdict: the checker cannot see across the does> split, so the
\ labeled engine exit IS the permanent contract).
: GE-DOES-WIDE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   GE-ILAYOUT-PRELUDE
   s" : GE-WDOES ( gewide<n,n> -- gewide<n,n> ) dup drop create does> ( ptr a -- n ) drop 5 ;" GE-SRC-LINE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   75 s" does>-split wide facts fail closed" GE-EXPECT-RC
   s" does>-split cannot lower layout width facts" s" does>-split wide diagnostic" GE-EXPECT-ERR-HAS ;

: GE-INTERP-LAYOUT ( -- )
   s" GE-WMK dup . . . ." s" interp layout dup fails closed" GE-ILAYOUT-CASE
   s" GE-WMK drop ." s" interp layout drop fails closed" GE-ILAYOUT-CASE
   s" 5 GE-WMK swap . . ." s" interp layout swap fails closed" GE-ILAYOUT-CASE
   s" ' GE-WMK execute" s" interp layout tick fails closed" GE-ILAYOUT-CASE
   s" : GE-WMK2 ( -- gewide<n,n> ) GE-WMK ; GE-WMK2 drop ." s" interp layout checked producer fails closed" GE-ILAYOUT-CASE
   s" defer GE-WD ( -- gewide<n,n> ) GE-WD" s" interp layout defer fails closed" GE-ILAYOUT-CASE
   GE-DOES-WIDE
   GE-ILAYOUT-GUARD
   GE-ILAYOUT-SCALAR
   s" PASS: interpret-mode layout transports fail closed" type cr ;

\ item 10 slice 2: `construct family variant` LOWERS in the native compiler —
\ (M-p) zero pads + tag as VS constants, the same literal path the item-8
\ generated-constructor bodies compile through, so the two intro forms are
\ indistinguishable at runtime. The execution fixture proves round-trips
\ cell-for-cell against the generated word across one-payload, wide
\ (max-payload), and zero-payload variants of an arbitrary family (gecn — not
\ result/option/color); the unpack is a generated TRUSTED boundary confined to
\ the temp fixture source (checked code cannot read raw bundle cells until
\ MATCH lowers in slice 3). Interpret-mode construct stays fail-closed
\ (E-UNDEFINED: compile-only keyword; the DNAME-WIDE gate owns the interpret
\ surface), and owner-only scope holds at compile: a foreign-package public
\ family and an unknown variant die with the named engine rejects.
: GE-CONSTRUCT-EXEC-SRC ( -- )          \ shared family + unpack + printer prelude
   s" SUMTYPE gecn 0" GE-SRC-LINE
   s"   VARIANT one n ;VARIANT" GE-SRC-LINE
   s"   VARIANT two n n ;VARIANT" GE-SRC-LINE
   s"   VARIANT nil ;VARIANT" GE-SRC-LINE
   s" ;SUMTYPE" GE-SRC-LINE
   s" TRUSTED: GE-UN3 ( gecn -- n n n ) ;" GE-SRC-LINE
   s" : GE-P3 ( gecn -- ) GE-UN3 . . . ;" GE-SRC-LINE ;

: GE-CONSTRUCT-ROUND ( -- )             \ construct == generated ctor, cell-for-cell
   GE-HB-RESET
   GE-SRC-RESET
   GE-CONSTRUCT-EXEC-SRC
   s" : GE-MK1 ( n -- gecn ) construct gecn one ;" GE-SRC-LINE
   s" : GE-MK2 ( n n -- gecn ) construct gecn two ;" GE-SRC-LINE
   s" : GE-MK0 ( -- gecn ) construct gecn nil ;" GE-SRC-LINE
   s" : GE-T1 ( -- ) 7 GE-MK1 GE-P3 ;  GE-T1" GE-SRC-LINE
   s" : GE-T2 ( -- ) 3 4 GE-MK2 GE-P3 ;  GE-T2" GE-SRC-LINE
   s" : GE-T0 ( -- ) GE-MK0 GE-P3 ;  GE-T0" GE-SRC-LINE
   s" : GE-G1 ( -- ) 7 GECN:ONE GE-P3 ;  GE-G1" GE-SRC-LINE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" construct lowering executes" GE-EXPECT-OK
   SB-RESET                              \ stack prints top-first: tag, slot1, slot0
   s" 0" SB-APPEND GE-SB-LF  s" 0" SB-APPEND GE-SB-LF  s" 7" SB-APPEND GE-SB-LF
   s" 1" SB-APPEND GE-SB-LF  s" 4" SB-APPEND GE-SB-LF  s" 3" SB-APPEND GE-SB-LF
   s" 2" SB-APPEND GE-SB-LF  s" 0" SB-APPEND GE-SB-LF  s" 0" SB-APPEND GE-SB-LF
   s" 0" SB-APPEND GE-SB-LF  s" 0" SB-APPEND GE-SB-LF  s" 7" SB-APPEND GE-SB-LF
   SB$ s" construct round-trip cells" GE-EXPECT-OUT ;

: GE-CONSTRUCT-BAD-VARIANT ( -- )       \ unknown variant dies at ITS token
   GE-HB-RESET
   GE-SRC-RESET
   GE-CONSTRUCT-EXEC-SRC
   s" : GE-BADV ( n -- gecn ) construct gecn nope ;" GE-SRC-LINE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   70 s" unknown construct variant fails closed" GE-EXPECT-RC
   s" hb: construct: unknown variant: nope" s" construct variant diagnostic" GE-EXPECT-ERR-HAS ;

: GE-CONSTRUCT-FOREIGN ( -- )           \ owner-only: a foreign public family never lowers
   GE-HB-RESET
   GE-SRC-RESET
   s" package gepk" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" SUMTYPE gefr 0" GE-SRC-LINE
   s"   VARIANT yes n ;VARIANT" GE-SRC-LINE
   s" ;SUMTYPE" GE-SRC-LINE
   s" end-package" GE-SRC-LINE
   s" : GE-BADF ( n -- gefr ) construct gefr yes ;" GE-SRC-LINE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   70 s" foreign-package construct fails closed" GE-EXPECT-RC
   s" hb: construct: unknown family: gefr" s" construct foreign-family diagnostic" GE-EXPECT-ERR-HAS ;

: GE-CONSTRUCT-EXEC ( -- )
   GE-CONSTRUCT-ROUND
   GE-CONSTRUCT-BAD-VARIANT
   GE-CONSTRUCT-FOREIGN
   s" construct" 70 s" interpret construct fails closed" GE-UNCAUGHT-RUN
   s" PASS: construct lowers natively; interpret + foreign scope stay fail-closed" type cr ;

\ item 10 slice 3: `MATCH family v OF ... ENDOF ... ;MATCH` LOWERS in the native
\ compiler — peek tag / compare-branch chain / per-variant prologue (drop tag +
\ M-p pads, expose the p payload cells) / ENDOF jump-to-join / ;MATCH join +
\ invalid-tag die. The family gemt (one n / two n n / nil, M=2) is an arbitrary
\ third sum, not result/option/color. The round-trip drives one/two/nil so the
\ zero-, one-, and multi-payload prologues are all exercised and the payload cells
\ arrive in order; a nested MATCH proves the token machine and the fam stack
\ restore across ;MATCH. A forged tag (TRUSTED constructor with an out-of-range
\ tag) reaches the die IN A CHILD PROCESS (a die exits the engine): rc E-BAD-TAG
\ (85) + the inline "hb: bad gemt tag" diagnostic. Compile-time rejects
\ (unknown variant / a token where OF was required) die fail-closed at their
\ token, and interpret-mode MATCH stays E-UNDEFINED (compile-only keyword; the
\ DNAME-WIDE gate owns the interpret surface).
: GE-MATCH-EXEC-SRC ( -- )              \ shared matchable family (arbitrary third sum)
   s" SUMTYPE gemt 0" GE-SRC-LINE
   s"   VARIANT one n ;VARIANT" GE-SRC-LINE
   s"   VARIANT two n n ;VARIANT" GE-SRC-LINE
   s"   VARIANT nil ;VARIANT" GE-SRC-LINE
   s" ;SUMTYPE" GE-SRC-LINE ;

: GE-MATCH-ROUND ( -- )                 \ construct+MATCH round-trip, each variant + payload
   GE-HB-RESET
   GE-SRC-RESET
   GE-MATCH-EXEC-SRC
   s" : RN ( gemt -- n ) MATCH gemt one OF ENDOF two OF + ENDOF nil OF 999 ENDOF ;MATCH ;" GE-SRC-LINE
   s" : RP ( gemt -- ) MATCH gemt one OF . ENDOF two OF . . ENDOF nil OF 111 . ENDOF ;MATCH ;" GE-SRC-LINE
   s" : GN ( -- ) 7 construct gemt one RN .  3 4 construct gemt two RN .  construct gemt nil RN . ;  GN" GE-SRC-LINE
   s" : GP ( -- ) 5 construct gemt one RP  8 9 construct gemt two RP  construct gemt nil RP ;  GP" GE-SRC-LINE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" match lowering executes" GE-EXPECT-OK
   SB-RESET
   s" 7" SB-APPEND GE-SB-LF  s" 7" SB-APPEND GE-SB-LF  s" 999" SB-APPEND GE-SB-LF   \ RN one/two/nil
   s" 5" SB-APPEND GE-SB-LF                                                          \ RP one payload
   s" 9" SB-APPEND GE-SB-LF  s" 8" SB-APPEND GE-SB-LF                                \ RP two payload (top-first)
   s" 111" SB-APPEND GE-SB-LF                                                        \ RP nil branch
   SB$ s" match round-trip output" GE-EXPECT-OUT ;

: GE-MATCH-NESTED ( -- )                \ MATCH nested inside a MATCH branch body
   GE-HB-RESET
   GE-SRC-RESET
   GE-MATCH-EXEC-SRC
   s" : RNEST ( gemt -- n )" GE-SRC-LINE
   s"    MATCH gemt" GE-SRC-LINE
   s"      one OF construct gemt nil MATCH gemt one OF drop ENDOF two OF drop drop ENDOF nil OF ENDOF ;MATCH ENDOF" GE-SRC-LINE
   s"      two OF + ENDOF" GE-SRC-LINE
   s"      nil OF 0 ENDOF" GE-SRC-LINE
   s"    ;MATCH ;" GE-SRC-LINE
   s" : GO ( -- ) 7 construct gemt one RNEST .  3 4 construct gemt two RNEST .  construct gemt nil RNEST . ;  GO" GE-SRC-LINE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" nested match lowering executes" GE-EXPECT-OK
   SB-RESET  s" 7" SB-APPEND GE-SB-LF  s" 7" SB-APPEND GE-SB-LF  s" 0" SB-APPEND GE-SB-LF
   SB$ s" nested match output" GE-EXPECT-OUT ;

: GE-MATCH-BAD-TAG ( -- )               \ forged tag dies E-BAD-TAG in a child process
   GE-HB-RESET
   GE-SRC-RESET
   GE-MATCH-EXEC-SRC
   s" TRUSTED: GE-FORGE ( -- gemt ) 0 0 5 ;" GE-SRC-LINE
   s" : RN ( gemt -- n ) MATCH gemt one OF ENDOF two OF + ENDOF nil OF 0 ENDOF ;MATCH ;" GE-SRC-LINE
   s" : GO ( -- ) GE-FORGE RN . ;  GO" GE-SRC-LINE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   85 s" forged tag dies with E-BAD-TAG" GE-EXPECT-RC
   s" hb: bad gemt tag" s" bad-tag diagnostic" GE-EXPECT-ERR-HAS ;

: GE-MATCH-BAD-VARIANT ( -- )           \ unknown variant dies at ITS token
   GE-HB-RESET
   GE-SRC-RESET
   GE-MATCH-EXEC-SRC
   s" : Z ( gemt -- n ) MATCH gemt nope OF ENDOF ;MATCH ;" GE-SRC-LINE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   70 s" unknown match variant fails closed" GE-EXPECT-RC
   s" hb: match: unknown variant: nope" s" match variant diagnostic" GE-EXPECT-ERR-HAS ;

: GE-MATCH-EXPECTED-OF ( -- )           \ a variant not followed by OF dies fail-closed
   GE-HB-RESET
   GE-SRC-RESET
   GE-MATCH-EXEC-SRC
   s" : Z ( gemt -- n ) MATCH gemt one drop ;MATCH ;" GE-SRC-LINE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   70 s" match expected-of fails closed" GE-EXPECT-RC
   s" hb: match: expected of: drop" s" match expected-of diagnostic" GE-EXPECT-ERR-HAS ;

: GE-MATCH-EXEC ( -- )
   GE-MATCH-ROUND
   GE-MATCH-NESTED
   GE-MATCH-BAD-TAG
   GE-MATCH-BAD-VARIANT
   GE-MATCH-EXPECTED-OF
   s" match" 70 s" interpret match fails closed" GE-UNCAUGHT-RUN
   s" PASS: match lowers natively; forged tag dies E-BAD-TAG; interpret stays fail-closed" type cr ;

\ Dictionary-capacity exit diagnostic (dot habu-gate-runner-entry-81c84af0):
\ a tool closure needing more than DICT-CAP records died exit_group(77)
\ writing only the CURRENT TOKEN to fd 2 - a lone ':' byte, label-free and
\ unattributable. The definer capacity arms must emit a fixed label first:
\ `hb: dictionary full at: <token>`; rc 77 is the deterministic contract and
\ stays. The fixture is Habu-generated and scales with the baked DICT-CAP
\ (src/habu/layout.f is in the runtime prefix): DICT-CAP+1 unchecked trivial
\ definitions always overflow regardless of the boot dictionary count.
variable GE-DFULL-P                 \ generated-source cursor offset
variable GE-DFULL-DIV               \ decimal-render divisor
variable GE-DFULL-I                 \ copy/definition loop index

: GE-DFULL-C ( ptr u8 n -- ) {: buf:ptr c:n :}
   c buf GE-DFULL-P @ + c!
   GE-DFULL-P @ 1+ GE-DFULL-P ! ;

: GE-DFULL-S ( ptr u8 ptr u8 n -- ) {: buf:ptr a:ptr u:n :}
   0 GE-DFULL-I !
   begin GE-DFULL-I @ u < while
      buf  a GE-DFULL-I @ + c@  GE-DFULL-C
      GE-DFULL-I @ 1+ GE-DFULL-I !
   repeat ;

: GE-DFULL-DIGITS ( ptr u8 n -- ) {: buf:ptr i:n :}
   10000 GE-DFULL-DIV !
   begin GE-DFULL-DIV @ 0 > while
      buf  i GE-DFULL-DIV @ / 10 mod 48 +  GE-DFULL-C
      GE-DFULL-DIV @ 10 / GE-DFULL-DIV !
   repeat ;

: GE-DFULL-DEF ( ptr u8 n -- ) {: buf:ptr i:n :}      \ append `: wNNNNN ;\n`
   buf 58 GE-DFULL-C  buf 32 GE-DFULL-C  buf 119 GE-DFULL-C
   buf i GE-DFULL-DIGITS
   buf 32 GE-DFULL-C  buf 59 GE-DFULL-C  buf 10 GE-DFULL-C ;

: GE-DFULL-SOURCE ( -- ptr u8 n )                     \ generated define-past-cap program
   DICT-CAP 1+ 16 * 32 + MEM-ALLOC-BYTES drop {: buf:ptr :}
   0 GE-DFULL-P !
   buf s" 0 set-check" GE-DFULL-S  buf 10 GE-DFULL-C
   0 GE-DFULL-I !
   begin GE-DFULL-I @ DICT-CAP 1+ < while
      buf GE-DFULL-I @ GE-DFULL-DEF
      GE-DFULL-I @ 1+ GE-DFULL-I !
   repeat
   buf GE-DFULL-P @ ;

: GE-DICT-FULL ( -- )
   GE-DFULL-SOURCE {: src:ptr srcu:n :}
   GT-ROOT s" hb-dict-full.f" GE-SCRIPT-PATH JOIN-PATH GE-SCRIPT-U !
   GE-SCRIPT-PATH GE-SCRIPT-U @ src srcu WRITE-ALL
   GE-HB-RESET
   GE-HB$ GE-SCRIPT-PATH GE-SCRIPT-U @ GE-TIMEOUT-MS GE-RUN-STDIN-FILE
   77 s" dict-capacity exit rc" GE-EXPECT-RC
   s" hb: dictionary full at: " s" dict-capacity exit diagnostic" GE-EXPECT-ERR-HAS
   s" PASS: dictionary-capacity exit is labeled" type cr ;

: GE-DIV-MOD ( -- )
   GE-HB-RESET GE-SRC-RESET s" 1 0 / ." GE-SRC-LINE
   s" divide by zero trap" GE-HB-RUN-STDIN-NZ
   GE-HB-RESET GE-SRC-RESET s" 1 0 mod ." GE-SRC-LINE
   s" modulo by zero trap" GE-HB-RUN-STDIN-NZ
   GE-HB-RESET GE-SRC-RESET s" 7 2 / . 7 2 mod . cr" GE-SRC-LINE
   s" nonzero div/mod" GE-EVAL-RUN-STDIN
   SB-RESET s" 3" SB-APPEND GE-SB-LF s" 1" SB-APPEND GE-SB-LF GE-SB-LF
   SB$ s" nonzero div/mod output" GE-EXPECT-OUT
   s" PASS: div/mod by zero traps (no silent 0)" type cr ;

: GE-TRUST-SOURCE ( -- )
   GE-SRC-RESET
   s" w" GE-SRC-S"
   GE-SRC-SP
   s" n -- n" GE-SRC-S"
   s"  trust 7 . : Q 5 dup * . ; Q" GE-SRC-LINE ;

: GE-TRUST-RUN ( -- )
   GE-HB-RESET
   GE-TRUST-SOURCE
   s" checked hb trust/run smoke" GE-EVAL-RUN-STDIN
   SB-RESET s" 7" SB-APPEND GE-SB-LF s" 25" SB-APPEND GE-SB-LF
   SB$ s" checked hb trust/run smoke output" GE-EXPECT-OUT
   GE-HB-RESET
   GE-SRC-RESET
   s" HOME" GE-SRC-S"
   s"  getenv nip 0 > ." GE-SRC-LINE
   s" getenv" GE-EVAL-RUN-STDIN
   SB-RESET s" -1" SB-APPEND GE-SB-LF
   SB$ s" getenv output" GE-EXPECT-OUT ;

: GE-WRITE-SCRIPT-ARGV ( -- )
   GT-ROOT s" hb-script-argv.f" GE-SCRIPT-PATH JOIN-PATH GE-SCRIPT-U !
   GE-SRC-RESET
   s" SCRIPT-ARGC ." GE-SRC-LINE
   s" 0 SCRIPT-ARGV$ type cr" GE-SRC-LINE
   s" 1 SCRIPT-ARGV$ type cr" GE-SRC-LINE
   GE-SCRIPT-PATH GE-SCRIPT-U @ GE-SRC-BUF GE-SRC-U @ WRITE-ALL ;

: GE-ARGV-MODES ( -- )
   GE-WRITE-SCRIPT-ARGV
   GE-HB-RESET
   GE-SCRIPT-PATH GE-SCRIPT-U @ GE-ARG+
   s" alpha" GE-ARG+
   s" beta" GE-ARG+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" hb script argv mode" GE-EXPECT-OK
   SB-RESET s" 2" SB-APPEND GE-SB-LF s" alpha" SB-APPEND GE-SB-LF s" beta" SB-APPEND GE-SB-LF
   SB$ s" hb script argv mode output" GE-EXPECT-OUT
   GE-HB-RESET
   s" alpha" GE-ARG+
   s" beta" GE-ARG+
   GE-SRC-RESET
   s" ARGC ." GE-SRC-LINE
   s" 1 ARGV$ type cr" GE-SRC-LINE
   s" 2 ARGV$ type cr" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" hb pipeline argv mode" GE-EXPECT-OK
   SB-RESET s" 3" SB-APPEND GE-SB-LF s" alpha" SB-APPEND GE-SB-LF s" beta" SB-APPEND GE-SB-LF
   SB$ s" hb pipeline argv mode output" GE-EXPECT-OUT
   GE-HB-RESET
   GT-ROOT s" no-such-hb-script.f" GE-SCRIPT-PATH JOIN-PATH GE-SCRIPT-U !
   GE-SCRIPT-PATH GE-SCRIPT-U @ GE-ARG+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   74 s" hb missing script rc" GE-EXPECT-RC ;

: GE-GOOD-TYPED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : SQOK ( i64 -- i64 ) dup * ;" GE-SRC-LINE
   s" 7 SQOK ." GE-SRC-LINE
   s" hb good typed def" GE-EVAL-RUN-STDIN
   SB-RESET s" 49" SB-APPEND GE-SB-LF
   SB$ s" hb good typed def output" GE-EXPECT-OUT ;

: GE-BAD-TYPED ( -- )
   s" hb bad typed def" GT-PROGRESS-RUN
   s" inprocess-check" GS-EVENT
   [: s" SQBAD ( i64 -- i64 ) dup" CHECK-CANDIDATE! GE-BAD-TYPED-VERDICT ! ;]
   GE-CAPTURE-ACTION 0 <> if
      s" hb bad typed def" GE-FAIL
   then
   GE-BAD-TYPED-VERDICT @ 0 <> if s" hb bad typed def" GE-FAIL then
   s" sqbad" s" hb bad typed def" GE-EXPECT-ERR-HAS
   s" hb bad typed def" GT-PROGRESS-PASS ;

: GE-DEPTH ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GE-QDEPTH ( -- n ) depth ;" GE-SRC-LINE
   s" GE-QDEPTH ." GE-SRC-LINE
   s" hb depth prim certify+run" GE-EVAL-RUN-STDIN
   SB-RESET s" 0" SB-APPEND GE-SB-LF
   SB$ s" hb depth prim certify+run output" GE-EXPECT-OUT ;

: GE-TRUSTED-SOURCE ( -- )
   GE-SRC-RESET
   s" TRUSTED: TLEAK ( n -- n ) dup ;" GE-SRC-LINE
   s" TUSE ( n -- n ) TLEAK" GE-SRC-S"
   s"  CHECK! ." GE-SRC-LINE
   s" TBAD ( n -- n n ) TLEAK" GE-SRC-S"
   s"  CHECK! ." GE-SRC-LINE
   s" 5 TLEAK . ." GE-SRC-LINE ;

: GE-TRUSTED-EFFECT ( -- )
   GE-HB-RESET
   GE-TRUSTED-SOURCE
   s" hb TRUSTED: effect recording" GE-EVAL-RUN-STDIN
   SB-RESET
   s" -1" SB-APPEND GE-SB-LF
   s" 0" SB-APPEND GE-SB-LF
   s" 5" SB-APPEND GE-SB-LF
   s" 5" SB-APPEND GE-SB-LF
   SB$ s" hb TRUSTED: effect recording output" GE-EXPECT-OUT ;

: GE-SRC-TRUST ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n sig:ptr sigu:n :}
   name nameu GE-SRC-S"
   GE-SRC-SP
   sig sigu GE-SRC-S"
   s"  TRUST" GE-SRC-LINE ;

: GE-ROLE-SOURCE ( -- )
   GE-SRC-RESET
   s" -1 JSON-DIAGS !" GE-SRC-LINE
   s" DEFTYPE size" GE-SRC-LINE
   s" NEED-IDX" s" idx --" GE-SRC-TRUST
   s" NEED-LEN" s" len --" GE-SRC-TRUST
   s" NEED-SIZE" s" size --" GE-SRC-TRUST
   s" >SIZE" s" n -- size" GE-SRC-TRUST
   s" SIZE>N" s" size -- n" GE-SRC-TRUST
   s" GE-ROLE-ALL-CHECK ( n -- n ) >IDX IDX>N >LEN LEN>N >COUNT COUNT>N >OFF OFF>N >FD FD>N >RC RC>N >PID PID>N >MS MS>N >NS NS>N >TOK TOK>N >ASM ASM>N >IMG IMG>N >SNAP SNAP>N" GE-SRC-CHECK-LINE
   s" GE-ROLE-OK ( n -- ) >IDX NEED-IDX" GE-SRC-CHECK-LINE
   s" GE-ROLE-BAD ( n -- ) >IDX NEED-LEN" GE-SRC-CHECK-LINE
   s" GE-ROLE-BAD2 ( n -- n ) >LEN IDX>N" GE-SRC-CHECK-LINE
   s" GE-ROLE-BAD3 ( n -- img ) >ASM" GE-SRC-CHECK-LINE
   s" GE-SIZE-OK ( n -- n ) >SIZE SIZE>N" GE-SRC-CHECK-LINE
   s" GE-SIZE-BAD ( n -- ) >IDX NEED-SIZE" GE-SRC-CHECK-LINE
   s" GE-ROLE-UNKNOWN ( n -- unknownsize ) >IDX" GE-SRC-CHECK-LINE
   s" : GE-ROLE-ALL-RUN ( n -- n ) >IDX IDX>N >LEN LEN>N >COUNT COUNT>N >OFF OFF>N >FD FD>N >RC RC>N >PID PID>N >MS MS>N >NS NS>N >TOK TOK>N >ASM ASM>N >IMG IMG>N >SNAP SNAP>N ;" GE-SRC-LINE
   s" 7 GE-ROLE-ALL-RUN ." GE-SRC-LINE ;

: GE-ROLE-TYPES ( -- )
   GE-HB-RESET
   GE-ROLE-SOURCE
   s" hb nominal role types" GE-EVAL-RUN-STDIN
   SB-RESET
   s" -1" SB-APPEND GE-SB-LF
   s" -1" SB-APPEND GE-SB-LF
   s" 0" SB-APPEND GE-SB-LF
   s" 0" SB-APPEND GE-SB-LF
   s" 0" SB-APPEND GE-SB-LF
   s" -1" SB-APPEND GE-SB-LF
   s" 0" SB-APPEND GE-SB-LF
   s" 0" SB-APPEND GE-SB-LF
   s" 7" SB-APPEND GE-SB-LF
   SB$ s" hb nominal role output" GE-EXPECT-OUT
   s" E-MISMATCH" s" hb nominal role code" GE-EXPECT-ERR-HAS
   s" E-UNKNOWN-SIGNATURE-TYPE" s" hb unknown role code" GE-EXPECT-ERR-HAS
   s" unknownsize" s" hb unknown role token" GE-EXPECT-ERR-HAS
   s" fix_signature_type" s" hb unknown role repair class" GE-EXPECT-ERR-HAS
   s" expected" s" hb nominal role expected field" GE-EXPECT-ERR-HAS
   s" len" s" hb nominal role expected type" GE-EXPECT-ERR-HAS
   s" actual" s" hb nominal role actual field" GE-EXPECT-ERR-HAS
   s" idx" s" hb nominal role actual type" GE-EXPECT-ERR-HAS ;

: GE-TYPED-SMOKE ( -- )
   GE-GOOD-TYPED
   GE-BAD-TYPED
   GE-DEPTH
   GE-TRUSTED-EFFECT
   GE-ROLE-TYPES ;

: GE-TIMEOUT-ATTRIBUTION ( -- )
   GE-HB-RESET
   s" 1" GE-ARG+
   s" /bin/sleep" 50 GE-RUN-ENV
   GT-OUTCOME-KIND @ PROC-OUTCOME-TIMEOUT <> if
      s" gate timeout outcome attribution" GE-FAIL
   then
   GT-OUTCOME-CODE @ SIGKILL <> if
      s" gate timeout signal attribution" GE-FAIL
   then
   s" PASS: gate timeout outcome attribution" type cr ;

: GE-PROCESS-PTY ( -- )
   GE-HB-RESET
   s" --load" GE-ARG+
   s" lib/errors.f" GE-ARG+
   s" lib/process.f" GE-ARG+
   s" test/proc-pty.f" GE-ARG+
   s" --" GE-ARG+
   GE-HB$ GE-ARG+
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV
   s" process/pty" GE-EXPECT-OK
   s" PASS: process/pty primitives" s" process/pty output" GE-EXPECT-OUT-HAS
   s" PASS: process/pty primitives" type cr ;

: GE-UNDERFLOW-DIAG ( -- )
   \ A top-level interpreted line that consumes more cells than the data stack
   \ holds must fail closed: a named E-UNDERFLOW diagnostic (with the offending
   \ word) + exit 70, never a crash/signal. `drop @ .` is the worker's shape - the
   \ underflow (drop below S0) is caught at the interpret-loop boundary BEFORE the
   \ `@` deref that used to fault the garbage cell (crash handler exit 134).
   GE-HB-RESET
   GE-SRC-RESET
   s" drop @ ." GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   70 s" hb top-level underflow rc" GE-EXPECT-RC
   s" E-UNDERFLOW" s" hb top-level underflow diagnostic" GE-EXPECT-ERR-HAS
   s" drop" s" hb top-level underflow token" GE-EXPECT-ERR-HAS ;

: GE-DEREF-1 ( ptr u8 n -- ) {: tok:ptr toku:n :}
   \ Run one deref/execute primitive as the LITERAL FIRST top-level token on an
   \ empty stack: the pre-exec arity guard must name E-UNDERFLOW + exit 70, never a
   \ signal (crash handler exit 134). Before the guard this faulted inside the prim.
   GE-HB-RESET
   GE-SRC-RESET
   tok toku GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   70 s" hb deref-first arity rc" GE-EXPECT-RC
   s" E-UNDERFLOW" s" hb deref-first arity diagnostic" GE-EXPECT-ERR-HAS
   tok toku s" hb deref-first arity token" GE-EXPECT-ERR-HAS ;

: GE-DEREF-ARITY-DIAG ( -- )
   s" @" GE-DEREF-1
   s" !" GE-DEREF-1
   s" execute" GE-DEREF-1
   \ positive control: a valid store satisfies min-in -> succeeds rc 0 (no false guard).
   GE-HB-RESET
   GE-SRC-RESET
   s" variable GAV 5 GAV !" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" hb valid deref store succeeds" GE-EXPECT-OK ;

: GE-NESTED-DEF-SRC ( ptr u8 n -- ) {: body:ptr bodyu:n :}
   \ Build: TRUSTED: W ( -- ) s" <body>" evaluate ;  then run W.
   \ W is TRUSTED: because `evaluate` is an uncheckable metaprogramming boundary
   \ (its effect is dynamic); the definition compiled BY <body> is still fully
   \ checked by the active hook, from inside W's execution.
   GE-SRC-RESET
   s" TRUSTED: W ( -- )" GE-SRC+  GE-SRC-SP
   body bodyu GE-SRC-S"
   s"  evaluate ;" GE-SRC-LINE
   s" W" GE-SRC-LINE ;

: GE-NESTED-CHECKED-DEF ( -- )
   \ Checker reentrancy across the word-execution boundary: a checked colon
   \ definition compiled WHILE a word executes must certify + publish correctly.
   \ Proven: ZZ compiles under the active hook from inside W, then runs -> 5, rc 0.
   GE-HB-RESET
   s" : ZZ ( -- n ) 5 ;" GE-NESTED-DEF-SRC
   s" ZZ ." GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" hb nested checked def rc" GE-EXPECT-OK
   SB-RESET s" 5" SB-APPEND GE-SB-LF
   SB$ s" hb nested checked def output" GE-EXPECT-OUT
   s" PASS: nested checked def certifies + runs (reentrant hook)" type cr ;

: GE-NESTED-BAD-DEF ( -- )
   \ The nested definition is NOT trusted just because its definer word is: a
   \ bad-effect nested def compiled from inside an executing word must still be
   \ REJECTED (rc 70). Proven: BAD ( -- n ) drop is rejected at 'drop'.
   GE-HB-RESET
   s" : BAD ( -- n ) drop ;" GE-NESTED-DEF-SRC
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   70 s" hb nested bad def rc" GE-EXPECT-RC
   s" bad" s" hb nested bad def word" GE-EXPECT-ERR-HAS
   s" drop" s" hb nested bad def token" GE-EXPECT-ERR-HAS
   s" PASS: nested bad-effect def rejected from inside a word" type cr ;

: GE-EVAL-UNDEF-SRC ( -- )
   \ The dot reproducer: an undefined word aborts a nested `:`-compile INSIDE
   \ `evaluate` (called from GO via the TRUSTED evaluate wrapper). Mid-compile the
   \ JIT dict region is RW; the aborted definition must unwind cleanly, not fault.
   GE-SRC-RESET
   s" TRUSTED: EV ( ptr u8 n -- ) evaluate ;" GE-SRC-LINE
   s" : GO ( -- )" GE-SRC+  GE-SRC-SP
   s" : FOO ( -- ) UNDEFINED-WORD-XYZ ;" GE-SRC-S"
   s"  EV ;" GE-SRC-LINE ;

: GE-EVAL-UNDEF-CATCHABLE ( -- )
   \ Under an enclosing quotation catch, the aborted nested :-compile unwinds the
   \ eval frame (partial def dropped) and delivers a CATCHABLE throw (code 70) to
   \ the catch -> `. cr` prints 70 and the process exits 0. Was: native register
   \ dump / SIGBUS exit 134 (W^X: returned into RW dict code without restoring RX).
   GE-HB-RESET
   GE-EVAL-UNDEF-SRC
   s" : T1 ( -- ) [: GO ;] catch . cr ;" GE-SRC-LINE
   s" T1" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" hb eval-undef catch rc" GE-EXPECT-OK
   s" 70" s" hb eval-undef catch code" GE-EXPECT-OUT-HAS
   s" E-UNDEFINED" s" hb eval-undef catch diag" GE-EXPECT-ERR-HAS
   s" UNDEFINED-WORD-XYZ" s" hb eval-undef catch token" GE-EXPECT-ERR-HAS
   s" PASS: undefined in nested :-compile under catch -> catchable code 70, exit 0" type cr ;

: GE-EVAL-UNDEF-FAILCLOSED ( -- )
   \ Same mid-compile abort inside evaluate but NO handler: the throw finds no
   \ catch, so it fails closed with rc 70 + E-UNDEFINED (like the top-level LRDIE
   \ path), never a signal and never continuing past the abort.
   GE-HB-RESET
   GE-EVAL-UNDEF-SRC
   s" GO" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   70 s" hb eval-undef no-catch rc" GE-EXPECT-RC
   s" E-UNDEFINED" s" hb eval-undef no-catch diag" GE-EXPECT-ERR-HAS
   s" UNDEFINED-WORD-XYZ" s" hb eval-undef no-catch token" GE-EXPECT-ERR-HAS
   s" PASS: undefined in nested :-compile w/o catch -> fail-closed rc70" type cr ;

: GE-COMPILE-UNDEF-TOPLEVEL ( -- )
   \ The top-level undefined-in-:-compile path (EVALD==0, no eval frame) is
   \ unchanged by the eval-frame recovery fix: E-UNDEFINED + rc 70, never a signal.
   GE-HB-RESET
   GE-SRC-RESET
   s" : FOO ( -- ) UNDEFINED-WORD-XYZ ;" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   70 s" hb top-level undef-compile rc" GE-EXPECT-RC
   s" E-UNDEFINED" s" hb top-level undef-compile diag" GE-EXPECT-ERR-HAS
   s" UNDEFINED-WORD-XYZ" s" hb top-level undef-compile token" GE-EXPECT-ERR-HAS
   s" PASS: top-level undefined-in-compile fail-closed rc70 (unchanged)" type cr ;

: GE-EVAL-UNDEF-RECOVER ( -- )
   GE-EVAL-UNDEF-CATCHABLE
   GE-EVAL-UNDEF-FAILCLOSED
   GE-COMPILE-UNDEF-TOPLEVEL ;

: GE-SET-CHECK-NEG ( -- )
   \ set-check is fail-closed at install (dot habu-stdlib-check-hook-fd883aea): a
   \ non-zero argument outside the live JIT code window [DBASE, CP) dies with a
   \ NAMED rc-70 diagnostic instead of BLRing into garbage at the next publish.
   \ 1 (below DBASE) and `dbase@ HOOK-CELL + @` (a code word mis-read from the
   \ wrong CODE base) are the two RCA shapes; both must exit 70, never signal.
   GE-HB-RESET
   GE-SRC-RESET s" 1 set-check" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   70 s" hb set-check tiny-xt rc" GE-EXPECT-RC
   s" set-check: invalid checker xt" s" hb set-check tiny-xt diag" GE-EXPECT-ERR-HAS
   GE-HB-RESET
   GE-SRC-RESET s" dbase@ $1B0 + @ set-check" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   70 s" hb set-check dbase-garbage rc" GE-EXPECT-RC
   s" set-check: invalid checker xt" s" hb set-check dbase-garbage diag" GE-EXPECT-ERR-HAS
   s" PASS: set-check fail-closed on garbage xt (rc 70, named diagnostic)" type cr ;

: GE-RUNTIME-CHECKS ( -- )
   GE-UNCAUGHT-THROW
   GE-INTERP-LAYOUT
   GE-CONSTRUCT-EXEC
   GE-MATCH-EXEC
   GE-DICT-FULL
   GE-DIV-MOD
   GE-PROCESS-PTY
   GE-TRUST-RUN
   GE-ARGV-MODES
   GE-UNDERFLOW-DIAG
   GE-DEREF-ARITY-DIAG
   GE-NESTED-CHECKED-DEF
   GE-NESTED-BAD-DEF
   GE-EVAL-UNDEF-RECOVER
   GE-SET-CHECK-NEG
   GE-TYPED-SMOKE
   GE-TIMEOUT-ATTRIBUTION ;

: GENG-BUILD-SLICE ( -- )
   GE-BUILD-FIXPOINT
   GT-CLEANUP
   s" PASS: native engine build gate slice" type cr ;

: GE-CANDIDATE-VALIDATE ( -- )
   s" candidate-validate" GS-EVENT
   GE-CANDIDATE!
   GE-EXPECT-CANDIDATE
   GE-CANDIDATE-SIZE-CHECK
   GE-ENGINE-SUITE
   GE-TYPE-FAMILY-SUITE
   GE-TYPE-DECL-SUITE
   GE-TYPE-CTOR-SUITE
   GE-TYPE-LINEAR-SUITE
   GE-TYPE-MATCH-SUITE
   GE-TYPE-LAYOUT-SUITE ;

: GENG-VALIDATE-SLICE ( -- )
   s" hb-gate-engine-validate" GT-START
   GE-CANDIDATE-VALIDATE
   GT-CLEANUP
   s" PASS: native engine candidate validation slice" type cr ;

: GENG-FIXTURES-SLICE ( -- )
   s" hb-gate-engine-fixtures" GT-START
   GE-RUN-STD-FIXTURES
   GT-CLEANUP
   s" PASS: native engine fixture gate slice" type cr ;

: GENG-REPAIR-SLICE ( -- )
   s" hb-gate-engine-repair" GT-START
   GE-REPAIR-HINTS-RUN
   GT-CLEANUP
   s" PASS: native engine repair gate slice" type cr ;

: GENG-RUNTIME-SLICE ( -- )
   s" hb-gate-engine-runtime" GT-START
   GE-RUNTIME-CHECKS
   GT-CLEANUP
   s" PASS: native engine runtime gate slice" type cr ;

: GE-MAIN ( -- )
   GENG-PARSE-SLICE
   GENG-SLICE @ GENG-BUILD-ID = if GENG-BUILD-SLICE exit then
   GENG-SLICE @ GENG-FIXTURES-ID = if GENG-FIXTURES-SLICE exit then
   GENG-SLICE @ GENG-REPAIR-ID = if GENG-REPAIR-SLICE exit then
   GENG-SLICE @ GENG-RUNTIME-ID = if GENG-RUNTIME-SLICE exit then
   GENG-SLICE @ GENG-VALIDATE-ID = if GENG-VALIDATE-SLICE exit then
   GE-BUILD-FIXPOINT
   GE-RUN-EXTRA-FIXTURES
   GE-CANDIDATE-VALIDATE
   GE-RUNTIME-CHECKS
   GT-CLEANUP
   s" PASS: native engine gate phase" type cr ;
