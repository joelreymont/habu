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

variable GE-SCRIPT-U
variable GE-CAND-U
variable GENG-SLICE
variable GE-PROF-I
variable GE-REG-I
variable GE-JIT-I
variable GE-IMG-I
variable GE-IMG-BUILD-I
variable GE-HABU1-I

create GE-CHECK-OFF-LINE
10 c, 48 c, 32 c, 115 c, 101 c, 116 c, 45 c,
99 c, 104 c, 101 c, 99 c, 107 c, 10 c,

: GENG-USAGE ( -- )
   s" usage: test/gate-engine.f [build|fixtures|repair|runtime]" GENG-USAGE-RC die ;

: GENG-ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: GENG-SLICE! ( n -- )
   GENG-SLICE ! ;

: GENG-PARSE-SLICE ( -- )
   GENG-ALL-ID GENG-SLICE!
   SCRIPT-ARGC 0= if exit then
   SCRIPT-ARGC 1 <> if GENG-USAGE then
   s" build" GENG-ARG0= if GENG-BUILD-ID GENG-SLICE! exit then
   s" fixtures" GENG-ARG0= if GENG-FIXTURES-ID GENG-SLICE! exit then
   s" repair" GENG-ARG0= if GENG-REPAIR-ID GENG-SLICE! exit then
   s" runtime" GENG-ARG0= if GENG-RUNTIME-ID GENG-SLICE! exit then
   GENG-USAGE ;

GE-FILES: GE-FS-MUTATE-RUN-FILES
   lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f
   lib/fs-mutate-test.f
;GE-FILES

GE-FILES: GE-FS-MUTATE-CHECK-FILES
   lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f
;GE-FILES

GE-FILES: GE-PROCESS-ARGV-RUN-FILES
   lib/errors.f lib/test.f lib/memory.f lib/process.f lib/process-argv.f
   lib/process-argv-test.f
;GE-FILES

GE-FILES: GE-PROCESS-ARGV-CHECK-FILES
   lib/errors.f lib/memory.f lib/process.f lib/process-argv.f
;GE-FILES

GE-FILES: GE-PROCESS-ENV-RUN-FILES
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/process.f
   lib/process-argv.f lib/process-env.f lib/process-env-test.f
;GE-FILES

GE-FILES: GE-PROCESS-ENV-CHECK-FILES
   lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/process.f lib/process-argv.f
   lib/process-env.f
;GE-FILES

GE-FILES: GE-PROCESS-CWD-RUN-FILES
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f lib/process-env.f lib/process-cwd.f
   lib/process-cwd-test.f
;GE-FILES

GE-FILES: GE-PROCESS-CWD-CHECK-FILES
   lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/process.f lib/process-argv.f
   lib/process-env.f lib/process-cwd.f
;GE-FILES

GE-FILES: GE-REPAIR-HINTS-RUN-FILES
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f tools/warm-run.f
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

: GE-FS-MUTATE-CHECK ( -- )
   GE-SRC-RESET
   [: GE-SRC-FILE+ ;] GE-FS-MUTATE-CHECK-FILES
   s" fs mutation stdlib check" GE-CHECK-SRC-LIST ;

: GE-PROCESS-ARGV-RUN ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-PROCESS-ARGV-RUN-FILES
   s" process argv stdlib" GE-HB-RUN ;

: GE-PROCESS-ARGV-CHECK ( -- )
   GE-SRC-RESET
   [: GE-SRC-FILE+ ;] GE-PROCESS-ARGV-CHECK-FILES
   s" process argv check" GE-CHECK-SRC-LIST ;

: GE-PROCESS-ENV-RUN ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-PROCESS-ENV-RUN-FILES
   s" process env stdlib" GE-HB-RUN ;

: GE-PROCESS-ENV-CHECK ( -- )
   GE-SRC-RESET
   [: GE-SRC-FILE+ ;] GE-PROCESS-ENV-CHECK-FILES
   s" process env check" GE-CHECK-SRC-LIST ;

: GE-PROCESS-CWD-RUN ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-PROCESS-CWD-RUN-FILES
   s" process cwd stdlib" GE-HB-RUN ;

: GE-PROCESS-CWD-CHECK ( -- )
   GE-SRC-RESET
   [: GE-SRC-FILE+ ;] GE-PROCESS-CWD-CHECK-FILES
   s" process cwd check" GE-CHECK-SRC-LIST ;

: GE-REPAIR-HINTS-RUN ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-REPAIR-HINTS-RUN-FILES
   s" repair diagnostic hints" GE-HB-RUN ;

: GE-HB-BASELINE-RUN ( -- )
   GE-LOAD-RESET
   [: GE-ARG+ ;] GE-HB-BASELINE-RUN-FILES
   s" hb baseline contracts" GE-HB-RUN ;

: GE-CANDIDATE! ( -- )
   GT-ROOT s" hb-new" GE-CAND-PATH JOIN-PATH GE-CAND-U ! ;

: GE-CANDIDATE$ ( -- ptr u8 n )
   GE-CAND-PATH GE-CAND-U @ ;

: GE-EXPECT-CANDIDATE ( -- )
   GE-CANDIDATE$ EXECUTABLE? 0= if
      s" hb-new candidate executable" GE-FAIL
   then ;

: GE-CHECK-OFF-LINE$ ( -- ptr u8 n )
   GE-CHECK-OFF-LINE 13 ;

: GE-OLD-HOOK$ ( -- ptr u8 n )
   SB-RESET
   s" : HOOK ( ptr u8 n -- n ) CHECK! " SB-APPEND
   s" dup -1 <> if 70 throw then ; ' HOOK set-check" SB-APPEND
   SB$ ;

: GE-STAGE2-HOOK$ ( -- ptr u8 n )
   s" ' HOOK set-check" ;

: GE-READ-BUILD-TMP ( ptr u8 n -- ptr u8 n ) {: name:ptr nameu :}
   name nameu BF-A$ FILE-SIZE MEM-ALLOC-64K-SPAN {: buf:ptr cap :}
   name nameu BF-A$ buf cap READ-ALL {: got :}
   buf got ;

: GE-SHAPE-HAS ( ptr u8 n ptr u8 n ptr u8 n -- ) {: a:ptr u needle:ptr needleu label:ptr labelu :}
   a u needle needleu CONTAINS? 0= if label labelu GE-FAIL then ;

: GE-SHAPE-LACKS ( ptr u8 n ptr u8 n ptr u8 n -- ) {: a:ptr u needle:ptr needleu label:ptr labelu :}
   a u needle needleu CONTAINS? if label labelu GE-FAIL then ;

: GE-SHAPE-FIND ( ptr u8 n ptr u8 n -- n ) {: a:ptr u needle:ptr needleu :}
   a u needle needleu FIND-SUB ;

: GE-SHAPE-FIND-AFTER ( ptr u8 n n ptr u8 n -- n ) {: a:ptr u start needle:ptr needleu :}
   start 0 < if -1 exit then
   start u >= if -1 exit then
   a start BYTE+ u start - needle needleu FIND-SUB
   dup 0 < if exit then
   start + ;

: GE-SHAPE-FOUND ( n ptr u8 n -- n ) {: pos label:ptr labelu :}
   pos 0 < if label labelu GE-FAIL then
   pos ;

: GE-SHAPE-NOT-FOUND ( n ptr u8 n -- )
   {: pos label:ptr labelu :}
   pos 0 >= if label labelu GE-FAIL then ;

: GE-STAGE2-SOURCE-SHAPE ( -- )
   s" stage2-src" GE-READ-BUILD-TMP {: a:ptr u :}
   a u GE-OLD-HOOK$ s" build stage2 stale hook" GE-SHAPE-LACKS
   a u s" HB-CHECK-HOOK" s" build stage2 duplicate hook def" GE-SHAPE-LACKS
   a u s" 0 set-check" s" build stage2 unchecked boundary" GE-SHAPE-HAS
   a u GE-STAGE2-HOOK$ s" build stage2 hook install" GE-SHAPE-HAS
   a u s" STDIN-OUT" s" build stage2 stdin output" GE-SHAPE-HAS ;

: GE-STAGE2-SCRATCH-SHAPE ( -- )
   BF-STAGE2-SOURCE
   s" stage2-src" GE-READ-BUILD-TMP {: a:ptr u :}
   a u s" S2-SOURCE-CAP allot" s" build stage2 static source buffer" GE-SHAPE-LACKS
   a u s" stage2: source mmap failed" s" build stage2 mmap source" GE-SHAPE-HAS ;

: GE-STAGE2-ORDER-SHAPE ( -- )
   s" stage2-src" GE-READ-BUILD-TMP {: a:ptr u :}
   a u s" : BPROF-ON" GE-SHAPE-FIND s" build stage2 prof" GE-SHAPE-FOUND GE-PROF-I !
   a u GE-PROF-I @ s" : EMIT-VRINIT" GE-SHAPE-FIND-AFTER s" build stage2 regalloc" GE-SHAPE-FOUND GE-REG-I !
   a u GE-REG-I @ s" : FOLD-ENTRY" GE-SHAPE-FIND-AFTER s" build stage2 jit" GE-SHAPE-FOUND GE-JIT-I !
   GE-PROF-I @ GE-REG-I @ >= if s" build stage2 prof/reg order" GE-FAIL then
   GE-REG-I @ GE-JIT-I @ >= if s" build stage2 reg/jit order" GE-FAIL then ;

: GE-STAGE2-IMAGE-SHAPE ( -- )
   s" stage2-src" GE-READ-BUILD-TMP {: a:ptr u :}
   a u s" : ASM-CODELEN!" GE-SHAPE-FIND s" build stage2 image token" GE-SHAPE-FOUND GE-IMG-I !
   a u GE-IMG-I @ s" : BUILD-IMAGE" GE-SHAPE-FIND-AFTER s" build stage2 image build" GE-SHAPE-FOUND GE-IMG-BUILD-I !
   a u GE-IMG-BUILD-I @ s" : RPD@" GE-SHAPE-FIND-AFTER s" build stage2 habu1 after image" GE-SHAPE-FOUND GE-HABU1-I !
   a u GE-IMG-BUILD-I @ GE-CHECK-OFF-LINE$ GE-SHAPE-FIND-AFTER s" build stage2 image unchecked span" GE-SHAPE-NOT-FOUND
   GE-IMG-I @ GE-IMG-BUILD-I @ >= if s" build stage2 image order" GE-FAIL then
   GE-IMG-BUILD-I @ GE-HABU1-I @ >= if s" build stage2 habu1 order" GE-FAIL then ;

: GE-SNAP-SOURCE-SHAPE ( -- )
   s" hb-snap-src" GE-READ-BUILD-TMP {: a:ptr u :}
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
   BF-TMP-RESET
   GE-CANDIDATE!
   GE-EXPECT-CANDIDATE
   s" PASS: self-rebuild fixpoint" type cr ;

: GE-RUN-STD-FIXTURES ( -- )
   GE-FS-MUTATE-RUN
   GE-FS-MUTATE-CHECK
   GE-PROCESS-ARGV-RUN
   GE-PROCESS-ARGV-CHECK
   GE-PROCESS-ENV-RUN
   GE-PROCESS-ENV-CHECK
   GE-PROCESS-CWD-RUN
   GE-PROCESS-CWD-CHECK
   GE-HB-BASELINE-RUN ;

: GE-RUN-EXTRA-FIXTURES ( -- )
   GE-RUN-STD-FIXTURES
   GE-REPAIR-HINTS-RUN ;

: GE-ENGINE-SUITE-ON ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu label:ptr labelu :}
   GE-HB-RESET
   GE-SRC-RESET
   s" test/engine-suite.f" GE-SRC-FILE+
   exe exeu GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-OK
   SB-RESET s" ok" SB-APPEND GE-SB-LF
   GT-OUT$ SB$ ENDS-WITH? 0= if label labelu GE-FAIL then
   s" PASS: " type label labelu type cr ;

: GE-ENGINE-SUITE ( -- )
   GE-CANDIDATE$ s" engine suite on hb-new" GE-ENGINE-SUITE-ON
   s" bin/hb" s" engine suite on bin/hb" GE-ENGINE-SUITE-ON ;

: GE-SNAPSHOT-HOOK-SOURCE ( -- )
   GE-SRC-RESET
   s" data-base $1B0 + @ 0= ." GE-SRC-LINE
   s" : SQOK ( i64 -- i64 ) dup * ;" GE-SRC-LINE
   s" 7 SQOK ." GE-SRC-LINE ;

: GE-SNAPSHOT-STDIN ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu label:ptr labelu :}
   exe exeu GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-OK ;

: GE-SNAPSHOT-HOOK-CHECK ( ptr u8 n -- ) {: exe:ptr exeu :}
   GE-HB-RESET
   GE-SNAPSHOT-HOOK-SOURCE
   exe exeu s" hb-new refresh/check hook" GE-SNAPSHOT-STDIN
   SB-RESET
   s" 0" GE-OUT-LINE
   s" 49" GE-OUT-LINE
   SB$ s" hb-new refresh/check hook output" GE-EXPECT-OUT ;

: GE-SNAPSHOT-LONG-SOURCE ( -- )
   GE-SRC-RESET
   s" ' spawn-argv-env-cwd-io drop 42 ." GE-SRC-LINE ;

: GE-SNAPSHOT-LONG-LOOKUP ( ptr u8 n -- ) {: exe:ptr exeu :}
   GE-HB-RESET
   GE-SNAPSHOT-LONG-SOURCE
   exe exeu s" hb-new long-name snapshot dictionary lookup" GE-SNAPSHOT-STDIN
   SB-RESET
   s" 42" GE-OUT-LINE
   SB$ s" hb-new long-name snapshot dictionary lookup output" GE-EXPECT-OUT ;

: GE-SNAPSHOT-CANDIDATE-CHECKS ( -- )
   GE-CANDIDATE$ GE-SNAPSHOT-HOOK-CHECK
   GE-CANDIDATE$ GE-SNAPSHOT-LONG-LOOKUP
   s" PASS: hb-new snapshot hook/dictionary coverage" type cr ;

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

: GE-SRC-TRUST ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu sig:ptr sigu :}
   name nameu GE-SRC-S"
   GE-SRC-SP
   sig sigu GE-SRC-S"
   s"  TRUST" GE-SRC-LINE ;

: GE-ROLE-SOURCE ( -- )
   GE-SRC-RESET
   s" -1 JSON-DIAGS !" GE-SRC-LINE
   s" NEED-IDX" s" idx --" GE-SRC-TRUST
   s" NEED-LEN" s" len --" GE-SRC-TRUST
   s" GE-ROLE-ALL-CHECK ( n -- n ) >IDX IDX>N >LEN LEN>N >COUNT COUNT>N >OFF OFF>N >FD FD>N >RC RC>N >PID PID>N >MS MS>N >NS NS>N >TOK TOK>N >ASM ASM>N >IMG IMG>N >SNAP SNAP>N" GE-SRC-CHECK-LINE
   s" GE-ROLE-OK ( n -- ) >IDX NEED-IDX" GE-SRC-CHECK-LINE
   s" GE-ROLE-BAD ( n -- ) >IDX NEED-LEN" GE-SRC-CHECK-LINE
   s" GE-ROLE-BAD2 ( n -- n ) >LEN IDX>N" GE-SRC-CHECK-LINE
   s" GE-ROLE-BAD3 ( n -- img ) >ASM" GE-SRC-CHECK-LINE
   s" GE-ROLE-UNKNOWN ( n -- size ) >IDX" GE-SRC-CHECK-LINE
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
   s" 0" SB-APPEND GE-SB-LF
   s" 7" SB-APPEND GE-SB-LF
   SB$ s" hb nominal role output" GE-EXPECT-OUT
   s" E-MISMATCH" s" hb nominal role code" GE-EXPECT-ERR-HAS
   s" E-UNKNOWN-SIGNATURE-TYPE" s" hb unknown role code" GE-EXPECT-ERR-HAS
   s" size" s" hb unknown role token" GE-EXPECT-ERR-HAS
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

GE-MAIN
