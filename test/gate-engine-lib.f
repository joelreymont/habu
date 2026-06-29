\ gate-engine.f - checked runner for engine and public hb gate checks.
\
\ Load after test/gate-common.f, lib/memory.f, lib/build.f, lib/codesign.f,
\ and tools/build-fixpoint.f.

64 constant GENG-USAGE-RC
0 constant GENG-ALL-ID
1 constant GENG-BUILD-ID
2 constant GENG-FIXTURES-ID
3 constant GENG-REPAIR-ID
4 constant GENG-RUNTIME-ID

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

create GE-CHECK-OFF-LINE
10 c, 48 c, 32 c, 115 c, 101 c, 116 c, 45 c,
99 c, 104 c, 101 c, 99 c, 107 c, 10 c,

: GENG-USAGE ( -- )
   s" usage: test/gate-engine.f [build|fixtures|repair|runtime] [--pool-slots N]" GENG-USAGE-RC die ;

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

GE-FILES: GE-FS-MUTATE-RUN-FILES
   lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f
   lib/fs-mutate-test.f
;GE-FILES

GE-FILES: GE-PROCESS-ARGV-RUN-FILES
   lib/errors.f lib/test.f lib/memory.f lib/process.f lib/process-argv.f
   lib/process-argv-test.f
;GE-FILES

GE-FILES: GE-PROCESS-ENV-RUN-FILES
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/process.f
   lib/process-argv.f lib/process-env.f lib/process-env-test.f
;GE-FILES

GE-FILES: GE-PROCESS-CWD-RUN-FILES
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f lib/process-env.f lib/process-cwd.f
   lib/process-cwd-test.f
;GE-FILES

GE-FILES: GE-ENGINE-STDLIB-CHECK-FILES
   lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f lib/process-env.f lib/process-cwd.f
;GE-FILES

GE-FILES: GE-REPAIR-HINTS-RUN-FILES
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/vector.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f tools/lint/text.f tools/lint/token.f
   tools/lint/lib.f tools/lint/json-writer.f tools/lint/source-lex.f
   tools/check-all-errors-core.f tools/warm-run.f
   tools/json.f tools/gate-json-assert-core.f tools/check-repair-hints-test.f
;GE-FILES

GE-FILES: GE-HB-BASELINE-RUN-FILES
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f tools/hb-baseline-contracts-test.f
;GE-FILES

: GE-LOAD-RESET ( -- )
   GE-HB-RESET
   s" --load" GE-ARG+ ;

: GE-FS-MUTATE-RUN ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-FS-MUTATE-RUN-FILES
   s" fs mutation stdlib" GE-HB-RUN ;

: GE-PROCESS-ARGV-RUN ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-PROCESS-ARGV-RUN-FILES
   s" process argv stdlib" GE-HB-RUN ;

: GE-PROCESS-ENV-RUN ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-PROCESS-ENV-RUN-FILES
   s" process env stdlib" GE-HB-RUN ;

: GE-PROCESS-CWD-RUN ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-PROCESS-CWD-RUN-FILES
   s" process cwd stdlib" GE-HB-RUN ;

: GE-ENGINE-STDLIB-CHECK ( -- )
   GE-SRC-RESET
   [: GE-SRC-FILE+ ;] GE-ENGINE-STDLIB-CHECK-FILES
   s" engine stdlib support check" GE-CHECK-SRC-LIST ;

: GE-REPAIR-HINTS-RUN ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-REPAIR-HINTS-RUN-FILES
   s" repair diagnostic hints" GE-HB-RUN ;

: GE-HB-BASELINE-RUN ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-HB-BASELINE-RUN-FILES
   s" hb baseline contracts" GE-BIN-HB-RUN ;

: GE-RUN-ENV-ASYNC ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n timeout:n label:ptr labelu:n :}
   s" helper-spawn" GS-EVENT
   PROC-ENV-INHERIT-MISSING
   path pathu label labelu timeout GT-POOL-START ;

: GE-HB-RUN-ASYNC ( ptr u8 n -- ) {: label:ptr labelu:n :}
   s" inner-hb-spawn" GS-EVENT
   s" boundary-test" GS-EVENT
   GE-HB$ GE-TIMEOUT-MS label labelu GE-RUN-ENV-ASYNC ;

: GE-BIN-HB-RUN-ASYNC ( ptr u8 n -- ) {: label:ptr labelu:n :}
   s" inner-hb-spawn" GS-EVENT
   s" boundary-test" GS-EVENT
   s" bin/hb" GE-TIMEOUT-MS label labelu GE-RUN-ENV-ASYNC ;

: GE-FS-MUTATE-RUN-ASYNC ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-FS-MUTATE-RUN-FILES
   s" fs mutation stdlib" GE-HB-RUN-ASYNC ;

: GE-PROCESS-ARGV-RUN-ASYNC ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-PROCESS-ARGV-RUN-FILES
   s" process argv stdlib" GE-HB-RUN-ASYNC ;

: GE-PROCESS-ENV-RUN-ASYNC ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-PROCESS-ENV-RUN-FILES
   s" process env stdlib" GE-HB-RUN-ASYNC ;

: GE-PROCESS-CWD-RUN-ASYNC ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-PROCESS-CWD-RUN-FILES
   s" process cwd stdlib" GE-HB-RUN-ASYNC ;

: GE-HB-BASELINE-RUN-ASYNC ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-HB-BASELINE-RUN-FILES
   s" hb baseline contracts" GE-BIN-HB-RUN-ASYNC ;

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
   GT-ROOT s" hb-new" GE-CAND-PATH JOIN-PATH GE-CAND-U ! ;

: GE-ENV-CANDIDATE? ( -- bool )
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop 0 0= 0= exit
   then
   GE-CANDIDATE-PATH!
   0 0= ;

: GE-CANDIDATE! ( -- )
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
   s" hb-new" BF-A$ GE-SRC-CANDIDATE-PATH! ;

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

: GE-SNAP-SOURCE-SHAPE ( -- )
   s" hb-snap-src" GE-READ-BUILD-TMP {: a:ptr u:n :}
   a u s" SNAP-MAGIC" s" build snapshot source magic" GE-SHAPE-HAS ;

: GE-BUILD-SOURCE-SHAPE ( -- )
   GE-STAGE2-SOURCE-SHAPE
   GE-STAGE2-ORDER-SHAPE
   GE-STAGE2-IMAGE-SHAPE
   GE-SNAP-SOURCE-SHAPE ;

: GE-BUILD-FIXPOINT ( -- )
   s" candidate-build" GS-EVENT
   s" hb-gate-engine" GT-START
   GT-ROOT BF-TMP!
   BF-PREFLIGHT
   BF-STAGE2-SOURCE
   GE-STAGE2-SCRATCH-SHAPE
   BF-STAGE-FIXPOINT-FROM-SOURCE
   BF-BUILD-STDIN-FROM-STAGE
   BF-BUILD-SNAP-FROM-STDIN
   GE-BUILD-SOURCE-SHAPE
   GE-PROMOTE-CANDIDATE
   BF-TMP-RESET
   GE-EXPECT-CANDIDATE
   s" PASS: self-rebuild fixpoint" type cr ;

: GE-RUN-STD-FIXTURES ( -- )
   GT-POOL-RESET
   GE-FS-MUTATE-RUN-ASYNC
   GE-PROCESS-ARGV-RUN-ASYNC
   GE-PROCESS-ENV-RUN-ASYNC
   GE-PROCESS-CWD-RUN-ASYNC
   GE-HB-BASELINE-RUN-ASYNC
   GE-ENGINE-STDLIB-CHECK
   GT-POOL-DRAIN ;

: GE-RUN-EXTRA-FIXTURES ( -- )
   GE-RUN-STD-FIXTURES
   GE-REPAIR-HINTS-RUN ;

: GE-ENGINE-SUITE-ON ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   s" test/engine-suite.f" GE-SRC-FILE+
   exe exeu GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-OK
   SB-RESET s" ok" SB-APPEND GE-SB-LF
   GT-OUT$ SB$ ENDS-WITH? 0= if label labelu GE-FAIL then
   s" PASS: " type label labelu type cr ;

: GE-ENGINE-SUITE ( -- )
   GE-CANDIDATE$ s" engine suite on Habu-under-test" GE-ENGINE-SUITE-ON
   s" bin/hb" s" engine suite on bin/hb" GE-ENGINE-SUITE-ON ;

: GE-SNAPSHOT-HOOK-SOURCE ( -- )
   GE-SRC-RESET
   s" data-base $1B0 + @ 0= ." GE-SRC-LINE
   s" : SQOK ( i64 -- i64 ) dup * ;" GE-SRC-LINE
   s" 7 SQOK ." GE-SRC-LINE ;

: GE-SNAPSHOT-STDIN ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n label:ptr labelu:n :}
   exe exeu GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-OK ;

: GE-SNAPSHOT-HOOK-CHECK ( ptr u8 n -- ) {: exe:ptr exeu:n :}
   GE-HB-RESET
   GE-SNAPSHOT-HOOK-SOURCE
   exe exeu s" Habu-under-test refresh/check hook" GE-SNAPSHOT-STDIN
   SB-RESET
   s" 0" GE-OUT-LINE
   s" 49" GE-OUT-LINE
   SB$ s" Habu-under-test refresh/check hook output" GE-EXPECT-OUT ;

: GE-SNAPSHOT-LONG-SOURCE ( -- )
   GE-SRC-RESET
   s" ' spawn-argv-env-cwd-io drop 42 ." GE-SRC-LINE ;

: GE-SNAPSHOT-LONG-LOOKUP ( ptr u8 n -- ) {: exe:ptr exeu:n :}
   GE-HB-RESET
   GE-SNAPSHOT-LONG-SOURCE
   exe exeu s" Habu-under-test long-name snapshot dictionary lookup" GE-SNAPSHOT-STDIN
   SB-RESET
   s" 42" GE-OUT-LINE
   SB$ s" Habu-under-test long-name snapshot dictionary lookup output" GE-EXPECT-OUT ;

: GE-SNAPSHOT-CANDIDATE-CHECKS ( -- )
   GE-CANDIDATE$ GE-SNAPSHOT-HOOK-CHECK
   GE-CANDIDATE$ GE-SNAPSHOT-LONG-LOOKUP
   s" PASS: Habu-under-test snapshot hook/dictionary coverage" type cr ;

: GE-DIV-MOD ( -- )
   GE-HB-RESET GE-SRC-RESET s" 1 0 / ." GE-SRC-LINE
   s" divide by zero trap" GE-HB-RUN-STDIN-NZ
   GE-HB-RESET GE-SRC-RESET s" 1 0 mod ." GE-SRC-LINE
   s" modulo by zero trap" GE-HB-RUN-STDIN-NZ
   GE-HB-RESET GE-SRC-RESET s" 7 2 / . 7 2 mod . cr" GE-SRC-LINE
   s" nonzero div/mod" GE-HB-RUN-STDIN
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
   s" checked hb trust/run smoke" GE-HB-RUN-STDIN
   SB-RESET s" 7" SB-APPEND GE-SB-LF s" 25" SB-APPEND GE-SB-LF
   SB$ s" checked hb trust/run smoke output" GE-EXPECT-OUT
   GE-HB-RESET
   GE-SRC-RESET
   s" HOME" GE-SRC-S"
   s"  getenv nip 0 > ." GE-SRC-LINE
   s" getenv" GE-HB-RUN-STDIN
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
   s" hb good typed def" GE-HB-RUN-STDIN
   SB-RESET s" 49" SB-APPEND GE-SB-LF
   SB$ s" hb good typed def output" GE-EXPECT-OUT ;

: GE-BAD-TYPED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : SQBAD ( i64 -- i64 ) dup ;" GE-SRC-LINE
   s" 7 SQBAD ." GE-SRC-LINE
   s" hb bad typed def" GE-HB-RUN-STDIN-NZ ;

: GE-DEPTH ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GE-QDEPTH ( -- n ) depth ;" GE-SRC-LINE
   s" GE-QDEPTH ." GE-SRC-LINE
   s" hb depth prim certify+run" GE-HB-RUN-STDIN
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
   s" hb TRUSTED: effect recording" GE-HB-RUN-STDIN
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
   s" hb nominal role types" GE-HB-RUN-STDIN
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

: GE-RUNTIME-CHECKS ( -- )
   GE-DIV-MOD
   GE-TRUST-RUN
   GE-ARGV-MODES
   GE-TYPED-SMOKE
   GE-TIMEOUT-ATTRIBUTION ;

: GENG-BUILD-SLICE ( -- )
   GE-BUILD-FIXPOINT
   GE-ENGINE-SUITE
   GE-SNAPSHOT-CANDIDATE-CHECKS
   GT-CLEANUP
   s" PASS: native engine build gate slice" type cr ;

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
   GE-BUILD-FIXPOINT
   GE-RUN-EXTRA-FIXTURES
   GE-ENGINE-SUITE
   GE-SNAPSHOT-CANDIDATE-CHECKS
   GE-RUNTIME-CHECKS
   GT-CLEANUP
   s" PASS: native engine gate phase" type cr ;
