\ gate-engine.f - checked runner for engine and public hb gate checks.
\
\ Load after test/gate-common.f, lib/memory.f, lib/build.f, lib/codesign.f,
\ and tools/build-fixpoint.f.

require test/gate-build-size.f
require test/gate-size-attribution-test.f \ SIZE-ATTR:VALIDATE + HOST-CODE-TEXT exact CODELEN ratchet
require test/gate-validation-worker.f
require lib/test/budget.f                 \ TEST-BUDGET:PERF-MS - runtime-slice ratchet calibration
require lib/adt/option.f                 \ option<CAD-NUM:index> STR:FIND-SUB consumer (switchover wave A)
require lib/type/deftype.f               \ DEFTYPE - declared-nominal role exemplar in the runtime role source

\ White-box CAD-NUM role reader (precedent: lib/string-test.f STR-T-IX>RAW):
\ reopen the unsealed CAD-NUM package to project the typed STR:FIND-SUB index
\ back to its raw cell, keeping the shape-find helpers byte-identical. A plain
\ checked word over the audited private INDEX>N projection - not a new boundary.
package CAD-NUM
public
: GE-IX>RAW ( CAD-NUM:index -- n ) INDEX>N ;
;package

64 constant GENG-USAGE-RC
67 constant GE-UNCAUGHT-RC       \ deterministic exit status for an uncaught top-level throw
70 constant GE-IDENTITY-RC
0 constant GENG-ALL-ID
1 constant GENG-BUILD-ID
2 constant GENG-FIXTURES-ID
3 constant GENG-REPAIR-ID
4 constant GENG-RUNTIME-ID
5 constant GENG-VALIDATE-ID
6 constant GENG-CONSTRUCT-ID
7 constant GENG-RUNTIME-PARITY-ID
$40000 constant GE-MAX-CANDIDATE-BYTES

create GE-SCRIPT-PATH FS-PATH-CAP allot
create GE-CAND-PATH FS-PATH-CAP allot
create GE-SRC-CAND-PATH FS-PATH-CAP allot
create GE-SZMAP-PATH FS-PATH-CAP allot

variable GE-SCRIPT-U
variable GE-CAND-U
variable GE-SRC-CAND-U
variable GE-SZMAP-U
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
   s" usage: test/gate-engine.f [build|fixtures|repair|runtime|runtime-parity|validate|construct-parity] [--pool-slots N]" GENG-USAGE-RC die ;

: GENG-ARG$ ( -- ptr u8 n )
   GENG-ARG-I @ SCRIPT-ARGV$ ;

: GENG-ARG-VALUE$ ( -- ptr u8 n )
   GENG-ARG-I @ 1+ SCRIPT-ARGC >= if GENG-USAGE then
   GENG-ARG-I @ 1+ SCRIPT-ARGV$ ;

: GENG-POS-NUM ( ptr u8 n -- n )
   STR>NUMBER? MATCH option
     none OF GENG-USAGE ENDOF
     some OF ENDOF
   ;MATCH
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
   GENG-ARG$ s" runtime-parity" STR= if GENG-RUNTIME-PARITY-ID GENG-SLICE! 0 0= exit then
   GENG-ARG$ s" validate" STR= if GENG-VALIDATE-ID GENG-SLICE! 0 0= exit then
   GENG-ARG$ s" construct-parity" STR= if GENG-CONSTRUCT-ID GENG-SLICE! 0 0= exit then
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
   s" LOWER-CERT-HOOK:INSTALL" ;

: GE-READ-BUILD-TMP ( ptr u8 n -- ptr u8 n ) {: name:ptr nameu:n :}
   name nameu BF-A$ FILE-SIZE MEM-ALLOC-64K-SPAN {: buf:ptr cap:n :}
   name nameu BF-A$ buf cap READ-ALL {: got:n :}
   buf got ;

: GE-SHAPE-HAS ( ptr u8 n ptr u8 n ptr u8 n -- ) {: a:ptr u:n needle:ptr needleu:n label:ptr labelu:n :}
   a u needle needleu CONTAINS? 0= if label labelu GE-FAIL then ;

: GE-SHAPE-LACKS ( ptr u8 n ptr u8 n ptr u8 n -- ) {: a:ptr u:n needle:ptr needleu:n label:ptr labelu:n :}
   a u needle needleu CONTAINS? if label labelu GE-FAIL then ;

: GE-SHAPE-FIND ( ptr u8 n ptr u8 n -- option<idx> ) {: a:ptr u:n needle:ptr needleu:n :}
   a u STR:LENGTH needle needleu STR:LENGTH STR:FIND-SUB MATCH option
     none OF OPTION:NONE ENDOF
     some OF CAD-NUM:GE-IX>RAW >IDX OPTION:SOME ENDOF
   ;MATCH ;

: GE-SHAPE-FIND-AFTER ( ptr u8 n n ptr u8 n -- option<idx> ) {: a:ptr u:n start:n needle:ptr needleu:n :}
   start 0 < if OPTION:NONE exit then
   start u >= if OPTION:NONE exit then
   a start BYTE+ u start - STR:LENGTH needle needleu STR:LENGTH STR:FIND-SUB MATCH option
     none OF OPTION:NONE ENDOF
     some OF CAD-NUM:GE-IX>RAW start + >IDX OPTION:SOME ENDOF
   ;MATCH ;

: GE-SHAPE-FOUND ( option<idx> ptr u8 n -- n ) {: label:ptr labelu:n :}
   MATCH option
     none OF label labelu GE-FAIL ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

: GE-SHAPE-NOT-FOUND ( option<idx> ptr u8 n -- )
   {: label:ptr labelu:n :}
   MATCH option
     none OF ENDOF
     some OF drop label labelu GE-FAIL ENDOF
   ;MATCH ;

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

\ --- Exact CODELEN ratchet (dot habu-gate-enforce-exact-6effb905) -----------
\ The whole-file ratchet (GB-SIZE-*) measures the page-rounded container, so up to
\ one page of __text growth (Linux 4 KiB, macOS 16 KiB) accumulates INVISIBLY
\ between commits. This closes it: re-run the freshly built metabuild host
\ (hb-stdin-mk, the stdin engine's emitter) with HABU_ENGINE_SIZE_MAP=1, capture
\ its byte-attribution map (one block, byte-identical to the candidate at the
\ fixpoint), and hold the candidate's measured SUM-TEXT to the committed CODE-TEXT
\ row for the running target - so any code growth needs a deliberate same-commit
\ row bump in test/gate-size-attribution-test.f, mirroring the GB-SIZE
\ grown/STALE-BASELINE semantics. SIZE-ATTR:VALIDATE then reconciles every
\ remaining byte (floor-dist, region rows, residue). A missing or unparseable map
\ fails closed (SIZE-REPORT:LOAD dies).

: GE-SZMAP$ ( -- ptr u8 n )                \ capture-root path for the candidate size map
   GT-ROOT s" hb-size-map" GE-SZMAP-PATH JOIN-PATH GE-SZMAP-U !
   GE-SZMAP-PATH GE-SZMAP-U @ ;

: GE-CODELEN-CAPTURE ( -- )
   s" candidate-size-map" GS-EVENT
   GE-HB-RESET
   s" HABU_ENGINE_SIZE_MAP" >LEN s" 1" >LEN PROC-ENV+
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   s" hb-stdin-mk" BF-A$ GE-TIMEOUT-MS GE-RUN-ENV
   s" candidate size-map capture" GE-EXPECT-OK
   GE-SZMAP$ GT-OUT$ WRITE-ALL ;

\ Directional failures mirror test/gate-build-size.f's GB-SIZE-*-FAIL, retargeted
\ at the CODE-TEXT (__text) row. The pure class->action map is GB-SIZE's, already
\ self-checked by GB-SIZE-SELF-CHECK.
: GE-CODELEN-GROWN-FAIL ( n n -- )
   GB-SIZE-PAIR. cr
   s" candidate CODELEN ratchet: __text grew past the CODE-TEXT row - bump it in test/gate-size-attribution-test.f in this commit" GE-FAIL ;

: GE-CODELEN-STALE-FAIL ( n n -- )
   s" STALE-BASELINE " type GB-SIZE-PAIR. cr
   s" candidate CODELEN ratchet: __text shrank below the CODE-TEXT row - lower it in test/gate-size-attribution-test.f in this commit" GE-FAIL ;

: GE-CODELEN-MISSING-FAIL ( n n -- )
   GB-SIZE-PAIR. cr
   s" candidate CODELEN ratchet: no CODE-TEXT row for this target - commit the measured __text to test/gate-size-attribution-test.f" GE-FAIL ;

: GE-CODELEN-ENFORCE ( n n -- ) {: sz:n base:n :}   \ measured-SUM-TEXT committed-row
   sz base GB-SIZE-CLASS GB-SIZE-ACTION
   case
      GB-SIZE-GROWN of sz base GE-CODELEN-GROWN-FAIL endof
      GB-SIZE-SHRUNK of sz base GE-CODELEN-STALE-FAIL endof
      GB-SIZE-MISSING of sz base GE-CODELEN-MISSING-FAIL endof
   endcase ;

: GE-CODELEN-SYNTH$ ( -- ptr u8 n )        \ synthetic map: SUM-TEXT = 300 + 8 = 308
   s\" main/startup 300\nbaked-source 8\ncontainer/header 100\n" ;

\ Both-direction wiring proof: parse a synthetic map, then classify its SUM-TEXT
\ against a matching row (OK), a lower row (GROWN), a higher row (SHRUNK), and no
\ row (MISSING) - reds off-baseline, greens on baseline, without a fake engine.
: GE-CODELEN-SELF-CHECK ( -- )
   GE-CODELEN-SYNTH$ SIZE-REPORT:LOAD-BYTES
   SIZE-REPORT:SUM-TEXT {: st:n :}
   st st    GB-SIZE-OK      GB-SIZE-CLASS-EXPECT
   st st 1- GB-SIZE-GROWN   GB-SIZE-CLASS-EXPECT
   st st 1+ GB-SIZE-SHRUNK  GB-SIZE-CLASS-EXPECT
   st 0     GB-SIZE-MISSING GB-SIZE-CLASS-EXPECT ;

: GE-CODELEN-RATCHET ( -- )
   GE-CODELEN-SELF-CHECK
   GE-CODELEN-CAPTURE
   GE-SZMAP$ SIZE-REPORT:LOAD
   SIZE-REPORT:SUM-TEXT SIZE-ATTR:HOST-CODE-TEXT GE-CODELEN-ENFORCE
   GE-SZMAP$ GE-CANDIDATE$ SIZE-ATTR:VALIDATE
   s" PASS: exact CODELEN ratchet (SUM-TEXT held to committed CODE-TEXT row)" type cr ;

\ --- Per-region __text budget ratchet (dot habu-enforce-native-region-1003651b) -
\ The exact-CODELEN ratchet above holds the __text TOTAL to its committed row, but a
\ region that grows while a sibling shrinks nets zero there and hides which emitter
\ moved. This holds EACH committed per-region budget (SIZE-ATTR:HOST-REGION-BUDGETS,
\ measured same-commit at the byte fixpoint) to the candidate's measured region
\ size, mirroring the GB-SIZE / CODE-TEXT directional semantics per region: a grown
\ region is bumped, a shrunk region is STALE, and the reject NAMES the region.
\ Coverage is bidirectional - a newly emitted region with no budget and a budget row
\ whose region vanished both fail closed, named. macOS budgets are owed
\ (HOST-REGION-BUDGETS-MEASURED? false): the ratchet reports the owed state and the
\ measured page-crossing prediction, skipping enforcement on that host exactly as
\ the census skips its owed target.
: GE-REGION-CLASS ( n n -- n ) {: m:n b:n :}
   m b > if GB-SIZE-GROWN exit then
   m b < if GB-SIZE-SHRUNK exit then
   GB-SIZE-OK ;

: GE-REGION-CLASS-EXPECT ( n n n -- ) {: m:n b:n want:n :}
   m b GE-REGION-CLASS want <> if
      s" candidate region ratchet classifier boundary" GE-FAIL
   then ;

: GE-N>SB ( n -- ) {: v:n :}
   v 10 >= if v 10 / recurse then
   v 10 mod [char] 0 + SB-APPEND-C ;

\ Named directional reject: region + which way it drifted + budget/candidate + the
\ owning-commit action. Returned as a string so the fixtures can prove it names the
\ region and baseline without tripping the die.
: GE-REGION-REJECT$ ( ptr u8 n n n n -- ptr u8 n ) {: na:ptr nu:n m:n b:n dir:n :}
   SB-RESET
   s" region " SB-APPEND na nu SB-APPEND
   dir GB-SIZE-GROWN = if s"  grew past budget " else s"  shrank below budget (STALE-BASELINE) " then SB-APPEND
   b GE-N>SB s"  to candidate " SB-APPEND m GE-N>SB
   s"  - update its row in test/gate-size-attribution-test.f this commit" SB-APPEND
   SB$ ;

: GE-REGION-GROWN-FAIL ( ptr u8 n n n -- ) {: na:ptr nu:n m:n b:n :}
   na nu m b GB-SIZE-GROWN GE-REGION-REJECT$ GE-FAIL ;

: GE-REGION-STALE-FAIL ( ptr u8 n n n -- ) {: na:ptr nu:n m:n b:n :}
   na nu m b GB-SIZE-SHRUNK GE-REGION-REJECT$ GE-FAIL ;

: GE-REGION-VANISHED-FAIL ( ptr u8 n n -- ) {: na:ptr nu:n b:n :}
   SB-RESET
   s" budgeted region " SB-APPEND na nu SB-APPEND
   s"  is no longer emitted (budget " SB-APPEND b GE-N>SB
   s" ) - remove its row from test/gate-size-attribution-test.f this commit" SB-APPEND
   SB$ GE-FAIL ;

: GE-REGION-UNBUDGETED-FAIL ( ptr u8 n n -- ) {: na:ptr nu:n m:n :}
   SB-RESET
   s" unbudgeted __text region " SB-APPEND na nu SB-APPEND
   s"  (measured " SB-APPEND m GE-N>SB
   s" ) - commit its budget row to test/gate-size-attribution-test.f this commit" SB-APPEND
   SB$ GE-FAIL ;

: GE-REGION-ENFORCE-ONE ( ptr u8 n n n -- ) {: na:ptr nu:n m:n b:n :}
   m b GE-REGION-CLASS
   case
      GB-SIZE-GROWN  of na nu m b GE-REGION-GROWN-FAIL endof
      GB-SIZE-SHRUNK of na nu m b GE-REGION-STALE-FAIL endof
   endcase ;

\ Forward: every committed budget row is present in the map and matches exactly; a
\ budget whose region vanished fails named.
: GE-REGION-STEP ( ptr u8 n n -- ) {: na:ptr nu:n b:n :}
   na nu SIZE-REPORT:FIND MATCH option
      none OF na nu b GE-REGION-VANISHED-FAIL ENDOF
      some OF {: m:n :} na nu m b GE-REGION-ENFORCE-ONE ENDOF
   ;MATCH ;

\ Reverse: every measured non-container map row has a committed budget; a newly
\ emitted region fails named with its measured byte count to commit.
: GE-REGION-COVER-ONE ( n -- ) {: i:n :}
   i SIZE-REPORT:CONTAINER? if exit then
   i SIZE-REPORT:NAME$ {: na:ptr nu:n :}
   na nu SIZE-ATTR:HOST-REGION-BUDGET-FIND MATCH option
      none OF na nu i SIZE-REPORT:VAL@ GE-REGION-UNBUDGETED-FAIL ENDOF
      some OF drop ENDOF
   ;MATCH ;

: GE-REGION-ENFORCE-ALL ( -- )
   [: GE-REGION-STEP ;] SIZE-ATTR:HOST-REGION-BUDGETS
   0 begin dup SIZE-REPORT:COUNT < while
      dup GE-REGION-COVER-ONE
      1+
   repeat drop ;

\ Red-first per boundary, target-agnostic: at-budget green, one-past and +4 red,
\ shrink STALE, and the reject names the region - proven without a fake engine.
: GE-REGION-SYNTH-CHECK ( -- )
   100 100 GB-SIZE-OK     GE-REGION-CLASS-EXPECT
   101 100 GB-SIZE-GROWN  GE-REGION-CLASS-EXPECT
   104 100 GB-SIZE-GROWN  GE-REGION-CLASS-EXPECT
    99 100 GB-SIZE-SHRUNK GE-REGION-CLASS-EXPECT
    96 100 GB-SIZE-SHRUNK GE-REGION-CLASS-EXPECT
   s" main/startup" 104 100 GB-SIZE-GROWN GE-REGION-REJECT$ s" main/startup" CONTAINS? 0= if
      s" candidate region ratchet reject omits region name" GE-FAIL
   then ;

\ +4 into EACH committed region rejects GROWN and the reject names that region;
\ at-budget stays green, -4 is STALE. Runs where budgets are measured (host Linux).
: GE-REGION-BOUNDARY-STEP ( ptr u8 n n -- ) {: na:ptr nu:n b:n :}
   b     b GB-SIZE-OK     GE-REGION-CLASS-EXPECT
   b 4 + b GB-SIZE-GROWN  GE-REGION-CLASS-EXPECT
   b 4 - b GB-SIZE-SHRUNK GE-REGION-CLASS-EXPECT
   na nu b 4 + b GB-SIZE-GROWN GE-REGION-REJECT$ na nu CONTAINS? 0= if
      s" candidate region ratchet reject omits region name" GE-FAIL
   then ;

: GE-REGION-SELF-CHECK ( -- )
   GE-REGION-SYNTH-CHECK
   [: GE-REGION-BOUNDARY-STEP ;] SIZE-ATTR:HOST-REGION-BUDGETS ;

: GE-REGION-RATCHET ( -- )
   GE-REGION-SELF-CHECK
   SIZE-ATTR:HOST-REGION-BUDGETS-MEASURED? 0= if
      s" per-region budgets owed for this target (measure on that host); page prediction only" type cr
      SIZE-ATTR:PAGE-CROSS-REPORT
      exit
   then
   GE-SZMAP$ SIZE-REPORT:LOAD
   GE-REGION-ENFORCE-ALL
   SIZE-ATTR:PAGE-CROSS-REPORT
   s" PASS: per-region __text budget ratchet (every region held to its committed budget)" type cr ;

\ --- self-check certification census ratchet (dot habu-census-assert-...) ----
\ STATUS.md carries one `Certified (<target>): N  Uncheckable: 0  Rejected: 0`
\ row per build target (the SSOT). This slice re-measures the current target's
\ assembled stage2 source with the same VERIFY scanner the certify uses
\ (VERIFY:CENSUS-COUNT) and fails closed if the current target's row drifts. The
\ other target's `owed` row is not asserted here, mirroring the per-target
\ CODELEN rows in test/gate-build-size.f.
variable GE-CEN-P

\ STATUS.md is a repo file read cwd-relative (the gate runs from the repo root,
\ same as the cwd-relative src/ reads in BF-STAGE2-SOURCE), not a build artifact
\ under GT-ROOT.
: GE-STATUS$ ( -- ptr u8 n )
   s" STATUS.md" FILE-SIZE MEM-ALLOC-64K-SPAN {: buf:ptr cap:n :}
   s" STATUS.md" buf cap READ-ALL {: got:n :}
   buf got ;

: GE-CENSUS-KEY$ ( -- ptr u8 n )
   SB-RESET
   s" Certified (" SB-APPEND
   BF-CENSUS-TARGET$ SB-APPEND
   s" ):" SB-APPEND
   SB$ ;

: GE-CENSUS-MEASURED ( -- n )
   s" stage2-src" GE-READ-BUILD-TMP VERIFY:CENSUS-COUNT ;

: GE-CEN-SKIP-SP ( ptr u8 n -- ) {: a:ptr u:n :}
   begin GE-CEN-P @ u < while
      a GE-CEN-P @ BYTE@ 32 <> if exit then
      GE-CEN-P @ 1+ GE-CEN-P !
   repeat ;

: GE-CEN-DIGIT-END ( ptr u8 n -- ) {: a:ptr u:n :}
   begin GE-CEN-P @ u < while
      a GE-CEN-P @ BYTE@ STR-DIGIT? 0= if exit then
      GE-CEN-P @ 1+ GE-CEN-P !
   repeat ;

: GE-CENSUS-PARSE ( ptr u8 n n -- option<n> ) {: a:ptr u:n keyend:n :}
   keyend GE-CEN-P !
   a u GE-CEN-SKIP-SP
   GE-CEN-P @ {: ds:n :}
   a u GE-CEN-DIGIT-END
   GE-CEN-P @ ds - {: du:n :}
   du 0= if OPTION:NONE exit then
   a ds BYTE+ du STR-PARSE-POS ;

: GE-CENSUS-DRIFT-FAIL ( n n -- ) {: d:n m:n :}
   s" census ratchet: STATUS.md Certified (" type BF-CENSUS-TARGET$ type
   s" ) is " type d . s" the self-check measured " type m .
   s" census ratchet: certification count drift - update the current-target row in STATUS.md this commit" GE-FAIL ;

: GE-CENSUS-RATCHET ( -- )
   GE-CENSUS-MEASURED {: m:n :}
   GE-STATUS$ {: a:ptr u:n :}
   GE-CENSUS-KEY$ {: k:ptr ku:n :}
   a u k ku GE-SHAPE-FIND MATCH option
     none OF
        s" census ratchet: no Certified (" type BF-CENSUS-TARGET$ type
        s" ) row in STATUS.md - commit the measured count " type m .
        s" census ratchet: current-target census row missing or owed" GE-FAIL
     ENDOF
     some OF IDX>N ku + {: keyend:n :}
        a u keyend GE-CENSUS-PARSE MATCH option
          none OF s" census ratchet: unparseable count on the current-target STATUS.md row" GE-FAIL ENDOF
          some OF {: d:n :} d m <> if d m GE-CENSUS-DRIFT-FAIL then ENDOF
        ;MATCH
     ENDOF
   ;MATCH
   a u s" Uncheckable: 0  Rejected: 0" CONTAINS? 0= if
      s" census ratchet: STATUS.md Uncheckable/Rejected not 0/0" GE-FAIL
   then
   s" PASS: certification census ratchet (" type BF-CENSUS-TARGET$ type
   s"  current target; other target owed)" type cr ;

: GE-BUILD-FIXPOINT ( -- )
   s" candidate-build" GS-EVENT
   s" hb-gate-engine" GT-START
   GT-ROOT BF-TMP!
   BF-PREFLIGHT
   BF-STAGE2-SOURCE
   GE-STAGE2-SCRATCH-SHAPE
   GE-CENSUS-RATCHET
   BF-STAGE-FIXPOINT-FROM-SOURCE
   BF-BUILD-STDIN-FROM-STAGE
   GE-BUILD-SOURCE-SHAPE
   GE-PROMOTE-CANDIDATE
   GE-CODELEN-RATCHET
   GE-REGION-RATCHET
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

package GENG
private

: RUNTIME-MARK$ ( -- ptr u8 n )
   s" GE-RUNTIME-SUBJECT-BEGIN" ;

: SHAPE-ONE-AFTER ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: a:ptr u:n start:n needle:ptr needleu:n label:ptr labelu:n :}
   a u start needle needleu GE-SHAPE-FIND-AFTER
      label labelu GE-SHAPE-FOUND 1+ {: next:n :}
   a u next needle needleu GE-SHAPE-FIND-AFTER
      label labelu GE-SHAPE-NOT-FOUND ;

: SHAPE-COUNT-AFTER ( ptr u8 n n ptr u8 n -- n )
   {: a:ptr u:n start:n needle:ptr needleu:n :}
   a u start needle needleu GE-SHAPE-FIND-AFTER MATCH option
     none OF 0 ENDOF
     some OF
        IDX>N 1+ {: next:n :}
        a u next needle needleu RECURSE 1+
     ENDOF
   ;MATCH ;

: ASSERT-DIRECT-SITES ( n -- ) {: start:n :}
   GE-SRC-BUF GE-SRC-U @ start s" RUNTIME-DIRECT:NO-HANDLER" s" runtime direct no-handler site" SHAPE-ONE-AFTER
   GE-SRC-BUF GE-SRC-U @ start s" RUNTIME-DIRECT:TRAP" s" runtime direct trap site" SHAPE-ONE-AFTER
   GE-SRC-BUF GE-SRC-U @ start s" RUNTIME-DIRECT:FILE-LOADER" s" runtime direct file-loader site" SHAPE-ONE-AFTER
   GE-SRC-BUF GE-SRC-U @ start s" RUNTIME-DIRECT:SCRIPT-ARGV" s" runtime direct script-argv site" SHAPE-ONE-AFTER
   GE-SRC-BUF GE-SRC-U @ start s" RUNTIME-DIRECT:PIPE-ARGV" s" runtime direct pipe-argv site" SHAPE-ONE-AFTER
   GE-SRC-BUF GE-SRC-U @ start s" RUNTIME-DIRECT:MISSING-SCRIPT" s" runtime direct missing-script site" SHAPE-ONE-AFTER
   GE-SRC-BUF GE-SRC-U @ start s" RUNTIME-DIRECT:TIMEOUT" s" runtime direct timeout site" SHAPE-ONE-AFTER
   GE-SRC-BUF GE-SRC-U @ start s" RUNTIME-DIRECT:PTY" s" runtime direct pty site" SHAPE-ONE-AFTER
   GE-SRC-BUF GE-SRC-U @ start s" RUNTIME-DIRECT:IDENTITY-WORKER" s" runtime direct identity-worker site" SHAPE-ONE-AFTER
   GE-SRC-BUF GE-SRC-U @ start s" RUNTIME-DIRECT:WORKER" s" runtime direct worker site" SHAPE-ONE-AFTER ;

: PIN-DIRECT-IMPL ( ptr u8 n ptr u8 n -- )
   {: needle:ptr needleu:n label:ptr labelu:n :}
   GE-SRC-BUF GE-SRC-U @ 0 needle needleu label labelu SHAPE-ONE-AFTER ;

: ASSERT-DIRECT-IMPL ( -- )
   S\" : PARITY ( ptr u8 n -- )\n   SOURCE ;" s" runtime direct parity implementation" PIN-DIRECT-IMPL
   S\" : NO-HANDLER ( ptr u8 n -- )\n   SOURCE ;" s" runtime direct no-handler implementation" PIN-DIRECT-IMPL
   S\" : TRAP ( ptr u8 n -- )\n   SOURCE ;" s" runtime direct trap implementation" PIN-DIRECT-IMPL
   S\" : FILE-LOADER ( ptr u8 n -- ) {: path:ptr pathu:n :}\n   NOTE\n   GE-HB$ path pathu GE-TIMEOUT-MS GE-RUN-STDIN-FILE ;" s" runtime direct file-loader implementation" PIN-DIRECT-IMPL
   S\" : SCRIPT-ARGV ( -- )\n   ENV ;" s" runtime direct script-argv implementation" PIN-DIRECT-IMPL
   S\" : PIPE-ARGV ( ptr u8 n -- )\n   SOURCE ;" s" runtime direct pipe-argv implementation" PIN-DIRECT-IMPL
   S\" : MISSING-SCRIPT ( -- )\n   ENV ;" s" runtime direct missing-script implementation" PIN-DIRECT-IMPL
   S\" : TIMEOUT ( ptr u8 n n -- )\n   NOTE\n   GE-RUN-ENV ;" s" runtime direct timeout implementation" PIN-DIRECT-IMPL
   S\" : PTY ( -- )\n   ENV ;" s" runtime direct PTY implementation" PIN-DIRECT-IMPL
   S\" : IDENTITY-WORKER ( -- )\n   ENV ;" s" runtime direct identity implementation" PIN-DIRECT-IMPL
   S\" : WORKER ( -- )\n   ENV ;" s" runtime direct worker implementation" PIN-DIRECT-IMPL ;

: ASSERT-NO-RAW-DIRECT ( n -- ) {: start:n :}
   GE-SRC-BUF GE-SRC-U @ start s" GE-RUN-STDIN" GE-SHAPE-FIND-AFTER
      s" runtime raw stdin direct site" GE-SHAPE-NOT-FOUND
   GE-SRC-BUF GE-SRC-U @ start s" GE-RUN-ENV" GE-SHAPE-FIND-AFTER
      s" runtime raw env direct site" GE-SHAPE-NOT-FOUND ;

: ASSERT-PARITY-DIRECT ( -- )
   GE-SRC-RESET
   s" test/runtime-subject.f" GE-SRC-FILE+
   GE-SRC-BUF GE-SRC-U @ 0 s" RUNTIME-DIRECT:PARITY" s" runtime parity direct site" SHAPE-ONE-AFTER
   GE-SRC-BUF GE-SRC-U @ 0 s" RUNTIME-DIRECT:" SHAPE-COUNT-AFTER 1 <>
      if s" runtime parity direct site total" GE-FAIL then
   GE-SRC-BUF GE-SRC-U @ 0 s" GE-RUN-STDIN" GE-SHAPE-FIND-AFTER
      s" runtime parity raw direct site" GE-SHAPE-NOT-FOUND ;

public

: ASSERT-RUNTIME-SUBJECT ( -- )
   GE-SRC-RESET
   s" test/gate-engine-lib.f" GE-SRC-FILE+
   GE-SRC-BUF GE-SRC-U @ RUNTIME-MARK$ GE-SHAPE-FIND
      s" runtime subject marker literal" GE-SHAPE-FOUND 1+ {: first:n :}
   GE-SRC-BUF GE-SRC-U @ first RUNTIME-MARK$ GE-SHAPE-FIND-AFTER
      s" runtime subject marker" GE-SHAPE-FOUND {: start:n :}
   GE-SRC-BUF GE-SRC-U @ start s" bin/hb" GE-SHAPE-FIND-AFTER
      s" runtime subject bypasses candidate" GE-SHAPE-NOT-FOUND
   ASSERT-DIRECT-IMPL
   start ASSERT-DIRECT-SITES
   start ASSERT-NO-RAW-DIRECT
   ASSERT-PARITY-DIRECT ;

;package

package RUNTIME-DIRECT
private

10 constant SUBJECT-MAX
2 constant OWNER-MAX

variable EXEC-N

: NOTE ( -- )
   EXEC-N @ 1+ EXEC-N ! ;

: SOURCE ( ptr u8 n -- ) {: src:ptr srcu:n :}
   NOTE
   GE-HB$ src srcu GE-TIMEOUT-MS GE-RUN-STDIN ;

: ENV ( -- )
   NOTE
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV ;

public

: RESET ( -- )
   0 EXEC-N ! ;

: EXEC# ( -- n )
   EXEC-N @ ;

: SUBJECT-LIMIT ( -- n )
   SUBJECT-MAX ;

: OWNER-LIMIT ( -- n )
   OWNER-MAX ;

: PARITY ( ptr u8 n -- )
   SOURCE ;

: NO-HANDLER ( ptr u8 n -- )
   SOURCE ;

: TRAP ( ptr u8 n -- )
   SOURCE ;

: FILE-LOADER ( ptr u8 n -- ) {: path:ptr pathu:n :}
   NOTE
   GE-HB$ path pathu GE-TIMEOUT-MS GE-RUN-STDIN-FILE ;

: SCRIPT-ARGV ( -- )
   ENV ;

: PIPE-ARGV ( ptr u8 n -- )
   SOURCE ;

: MISSING-SCRIPT ( -- )
   ENV ;

: TIMEOUT ( ptr u8 n n -- )
   NOTE
   GE-RUN-ENV ;

: PTY ( -- )
   ENV ;

: IDENTITY-WORKER ( -- )
   ENV ;

: WORKER ( -- )
   ENV ;

;package

require test/runtime-subject.f

\ GE-RUNTIME-SUBJECT-BEGIN: every executable under this marker must dispatch
\ through GE-HB$; the source-shape regression forbids the baseline path here.

package RUNTIME-RUNNER
private

defer RUN ( ptr u8 n -- )

: SUBJECT ( ptr u8 n -- )
   RUNTIME-SUBJECT:RUN ;

: PARITY ( ptr u8 n -- )
   RUNTIME-SUBJECT:PARITY ;

public

: SUBJECT! ( -- )
   [: SUBJECT ;] is RUN ;

: PARITY! ( -- )
   [: PARITY ;] is RUN ;

: SOURCE ( ptr u8 n -- )
   RUN ;

: BUFFER ( -- )
   GE-SRC-BUF GE-SRC-U @ RUN ;

;package

RUNTIME-RUNNER:SUBJECT!

\ The former GE-CAND-SMOKE (hook-installed / checked-compile-run / baked-word-
\ resolves) is now three T= probes inside test/engine-suite.f, so it rides the
\ shared candidate-validation worker instead of a second
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
   GE-SRC-BUF GE-SRC-U @ RUNTIME-DIRECT:NO-HANDLER
   want label labelu GE-EXPECT-RC ;

package RUNTIME-RUNNER
public

: LINE-RC ( ptr u8 n n ptr u8 n -- )
   {: src:ptr srcu:n want:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   src srcu GE-SRC-LINE
   BUFFER
   want label labelu GE-EXPECT-RC ;

;package

: GE-UNCAUGHT-CASE ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n want:n needle:ptr needleu:n label:ptr labelu:n :}
   src srcu want label labelu GE-UNCAUGHT-RUN
   needle needleu label labelu GE-EXPECT-ERR-HAS ;

: GE-UNCAUGHT-THROW ( -- )
   s" -2816 throw" GE-UNCAUGHT-RC s" uncaught throw code -2816"
      s" uncaught throw -2816 (kernel-masks-to-0)" GE-UNCAUGHT-CASE
   s" -2802 throw" GE-UNCAUGHT-RC s" uncaught throw code -2802"
      s" uncaught throw -2802 (kernel-masks-to-14)" GE-UNCAUGHT-CASE
   s" 70 throw" 70 s" uncaught throw 70 representable passthrough" RUNTIME-RUNNER:LINE-RC
   s" uncaught throw 70 representable passthrough" GE-EXPECT-SILENT
   s" : GEUT ( -- ) [: -2816 throw ;] catch . ;  GEUT" 0
      s" caught throw stays in-process rc 0" RUNTIME-RUNNER:LINE-RC
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
   RUNTIME-RUNNER:BUFFER
   70 label labelu GE-EXPECT-RC
   s" interpret-mode layout value" label labelu GE-EXPECT-ERR-HAS ;

: GE-ILAYOUT-GUARD ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   GE-ILAYOUT-PRELUDE
   s" TRUSTED: GE-WUN ( gewide<n,n> -- n n ) ;" GE-SRC-LINE
   s" : GE-WRUN ( -- n n ) GE-WMK GE-WUN ;" GE-SRC-LINE
   s" GE-WRUN . ." GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-RUNNER:BUFFER
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

package CONSTRUCT-RUNNER
private

defer RUN ( ptr u8 n -- )

: SUBJECT ( ptr u8 n -- )
   2drop
   GE-SRC-BUF GE-SRC-U @ RUNTIME-SUBJECT:RUN ;

: PARITY ( ptr u8 n -- )
   2drop
   GE-SRC-BUF GE-SRC-U @ RUNTIME-SUBJECT:PARITY ;

public

: SUBJECT! ( -- )
   [: SUBJECT ;] is RUN ;

: PARITY! ( -- )
   [: PARITY ;] is RUN ;

: SOURCE ( ptr u8 n -- )
   RUN ;

;package

CONSTRUCT-RUNNER:SUBJECT!

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
   s" construct lowering direct/subject parity" CONSTRUCT-RUNNER:SOURCE
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
   s" construct bad-variant direct/subject parity" CONSTRUCT-RUNNER:SOURCE
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
   s" ;package" GE-SRC-LINE
   s" : GE-BADF ( n -- gefr ) construct gefr yes ;" GE-SRC-LINE
   s" construct foreign direct/subject parity" CONSTRUCT-RUNNER:SOURCE
   70 s" foreign-package construct fails closed" GE-EXPECT-RC
   s" hb: construct: unknown family: gefr" s" construct foreign-family diagnostic" GE-EXPECT-ERR-HAS ;

: GE-CONSTRUCT-EXEC ( -- )
   GE-CONSTRUCT-ROUND
   GE-CONSTRUCT-BAD-VARIANT
   GE-CONSTRUCT-FOREIGN
   s" construct" 70 s" interpret construct fails closed" RUNTIME-RUNNER:LINE-RC
   s" PASS: construct lowers natively; interpret + foreign scope stay fail-closed" type cr ;

\ item 10 slice 3: `MATCH family v OF ... ENDOF ... ;MATCH` LOWERS in the native
\ compiler — peek tag / compare-branch chain / per-variant prologue (drop tag +
\ M-p pads, expose the p payload cells) / ENDOF jump-to-join / ;MATCH join +
\ invalid-tag die. The family gemt (one n / two n n / nil, M=2) is an arbitrary
\ third sum, not result/option/color. The round-trip drives one/two/nil so the
\ zero-, one-, and multi-payload prologues are all exercised and the payload cells
\ arrive in order; a nested MATCH proves the token machine and the fam stack
\ restore across ;MATCH. A forged tag (TRUSTED constructor with an out-of-range
\ tag) reaches the die IN A CHILD PROCESS (a die exits the engine): rc HB-ERROR:BAD-TAG
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
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-RUNNER:BUFFER
   s" nested match lowering executes" GE-EXPECT-OK
   SB-RESET  s" 7" SB-APPEND GE-SB-LF  s" 7" SB-APPEND GE-SB-LF  s" 0" SB-APPEND GE-SB-LF
   SB$ s" nested match output" GE-EXPECT-OUT ;

: GE-MATCH-BAD-TAG ( -- )               \ forged tag dies HB-ERROR:BAD-TAG in a child process
   GE-HB-RESET
   GE-SRC-RESET
   GE-MATCH-EXEC-SRC
   s" TRUSTED: GE-FORGE ( -- gemt ) 0 0 5 ;" GE-SRC-LINE
   s" : RN ( gemt -- n ) MATCH gemt one OF ENDOF two OF + ENDOF nil OF 0 ENDOF ;MATCH ;" GE-SRC-LINE
   s" : GO ( -- ) GE-FORGE RN . ;  GO" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   HB-ERROR:BAD-TAG s" forged tag dies with HB-ERROR:BAD-TAG" GE-EXPECT-RC
   s" hb: bad gemt tag" s" bad-tag diagnostic" GE-EXPECT-ERR-HAS ;

: GE-MATCH-BAD-VARIANT ( -- )           \ unknown variant dies at ITS token
   GE-HB-RESET
   GE-SRC-RESET
   GE-MATCH-EXEC-SRC
   s" : Z ( gemt -- n ) MATCH gemt nope OF ENDOF ;MATCH ;" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" unknown match variant fails closed" GE-EXPECT-RC
   s" hb: match: unknown variant: nope" s" match variant diagnostic" GE-EXPECT-ERR-HAS ;

: GE-MATCH-EXPECTED-OF ( -- )           \ a variant not followed by OF dies fail-closed
   GE-HB-RESET
   GE-SRC-RESET
   GE-MATCH-EXEC-SRC
   s" : Z ( gemt -- n ) MATCH gemt one drop ;MATCH ;" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" match expected-of fails closed" GE-EXPECT-RC
   s" hb: match: expected of: drop" s" match expected-of diagnostic" GE-EXPECT-ERR-HAS ;

: GE-MATCH-EXEC ( -- )
   GE-MATCH-ROUND
   GE-MATCH-NESTED
   GE-MATCH-BAD-TAG
   GE-MATCH-BAD-VARIANT
   GE-MATCH-EXPECTED-OF
   s" match" 70 s" interpret match fails closed" RUNTIME-RUNNER:LINE-RC
   s" PASS: match lowers natively; forged tag dies HB-ERROR:BAD-TAG; interpret stays fail-closed" type cr ;

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

: GE-DFULL-WRITE ( ptr u8 CAD-NUM:alloc-byte-len -- ) {: buf:ptr len :}   \ generate the define-past-cap program into the scoped buffer, then persist it
   0 GE-DFULL-P !
   buf s" 0 set-check" GE-DFULL-S  buf 10 GE-DFULL-C
   0 GE-DFULL-I !
   begin GE-DFULL-I @ DICT-CAP 1+ < while
      buf GE-DFULL-I @ GE-DFULL-DEF
      GE-DFULL-I @ 1+ GE-DFULL-I !
   repeat
   GE-SCRIPT-PATH GE-SCRIPT-U @ buf GE-DFULL-P @ WRITE-ALL ;

: GE-DICT-FULL ( -- )
   GT-ROOT s" hb-dict-full.f" GE-SCRIPT-PATH JOIN-PATH GE-SCRIPT-U !
   DICT-CAP 1+ 16 * 32 + MEM:BYTES-ALLOC-LEN [: GE-DFULL-WRITE ;] MEM:WITH-BYTES
   GE-HB-RESET
   GE-SCRIPT-PATH GE-SCRIPT-U @ RUNTIME-DIRECT:FILE-LOADER
   77 s" dict-capacity exit rc" GE-EXPECT-RC
   s" hb: dictionary full at: " s" dict-capacity exit diagnostic" GE-EXPECT-ERR-HAS
   s" PASS: dictionary-capacity exit is labeled" type cr ;

\ DP heap (allot/,/c,/definer) must stop below the profiler counter band reserved at
\ the top PROF-CNT-BYTES of the DATA region (layout.f). DP-CHECK (habu1.f) caps the
\ heap at DATA-SIZE - PROF-CNT-BYTES so a large allot + prof-on can never let profiler
\ writes corrupt user data (dot habu-bound-profiler-counter-235c5f48). Over-bound fails
\ closed NAMED "hb: data space out of range" on fd 2 (catchable rc-76 throw inside
\ evaluate, exit 76 at top level). RED discriminator: on the unfixed base an allot one
\ byte INTO the band SUCCEEDS silently (rc 0, no message) and clobbers a counter.
\ `data-base`/`DATA-SIZE`/`PROF-CNT-BYTES`/`here` are runtime words, so the boundary is
\ computed against the live band base.
: GE-DATA-FULL ( -- )
   \ one byte past the band base rejects (base: succeeds silently — the RED discriminator)
   s" data-base DATA-SIZE PROF-CNT-BYTES - + here - 1+ allot"
      76 s" data-space over-band exit rc" RUNTIME-RUNNER:LINE-RC
   s" hb: data space out of range" s" data-space over-band diagnostic" GE-EXPECT-ERR-HAS
   \ allot ending EXACTLY at the band base (DP == DATA + DATA-SIZE - PROF-CNT-BYTES, the
   \ max the <= bound admits) must SUCCEED and exit clean.
   s" data-base DATA-SIZE PROF-CNT-BYTES - + here - allot"
      0 s" data-space band-base allot succeeds" RUNTIME-RUNNER:LINE-RC
   s" PASS: data-space profiler-band cap is labeled + off-by-one boundary holds" type cr ;

: GE-DIV-TRAP ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n label:ptr labelu:n :}
   GE-HB-RESET GE-SRC-RESET src srcu GE-SRC-LINE
   GE-SRC-BUF GE-SRC-U @ RUNTIME-DIRECT:TRAP
   label labelu GE-EXPECT-NONZERO ;

: GE-DIV-MOD ( -- )
   s" 1 0 / ." s" divide by zero trap" GE-DIV-TRAP
   s" 1 0 mod ." s" modulo by zero trap" GE-DIV-TRAP
   GE-HB-RESET GE-SRC-RESET s" 7 2 / . 7 2 mod . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-RUNNER:BUFFER
   SB-RESET s" 7" SB-APPEND GE-SB-LF s" 25" SB-APPEND GE-SB-LF
   SB$ s" checked hb trust/run smoke output" GE-EXPECT-OUT
   GE-HB-RESET
   GE-SRC-RESET
   s" HOME" GE-SRC-S"
   s"  getenv nip 0 > ." GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-DIRECT:SCRIPT-ARGV
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
   GE-SRC-BUF GE-SRC-U @ RUNTIME-DIRECT:PIPE-ARGV
   s" hb pipeline argv mode" GE-EXPECT-OK
   SB-RESET s" 3" SB-APPEND GE-SB-LF s" alpha" SB-APPEND GE-SB-LF s" beta" SB-APPEND GE-SB-LF
   SB$ s" hb pipeline argv mode output" GE-EXPECT-OUT
   GE-HB-RESET
   GT-ROOT s" no-such-hb-script.f" GE-SCRIPT-PATH JOIN-PATH GE-SCRIPT-U !
   GE-SCRIPT-PATH GE-SCRIPT-U @ GE-ARG+
   RUNTIME-DIRECT:MISSING-SCRIPT
   74 s" hb missing script rc" GE-EXPECT-RC ;

: GE-GOOD-TYPED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : SQOK ( i64 -- i64 ) dup * ;" GE-SRC-LINE
   s" 7 SQOK ." GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-RUNNER:BUFFER
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
   s" /bin/sleep" 50 RUNTIME-DIRECT:TIMEOUT
   GT-TIMED-OUT @ 0= if
      s" gate timeout outcome attribution" GE-FAIL
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
   RUNTIME-DIRECT:PTY
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
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-RUNNER:BUFFER
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
   RUNTIME-RUNNER:BUFFER
   70 s" hb top-level undef-compile rc" GE-EXPECT-RC
   s" E-UNDEFINED" s" hb top-level undef-compile diag" GE-EXPECT-ERR-HAS
   s" UNDEFINED-WORD-XYZ" s" hb top-level undef-compile token" GE-EXPECT-ERR-HAS
   s" PASS: top-level undefined-in-compile fail-closed rc70 (unchanged)" type cr ;

: GE-EVAL-UNDEF-RECOVER ( -- )
   GE-EVAL-UNDEF-CATCHABLE
   GE-EVAL-UNDEF-FAILCLOSED
   GE-COMPILE-UNDEF-TOPLEVEL ;

: GE-EVAL-CATCH-SRC ( -- )
   \ The dot-pair reproducer wrapper (test/type-ctor-suite.f TCE-CATCH shape):
   \ a quotation catch over the audited INCLUDE-EVALUATE boundary. The caller
   \ appends one failing source string; the caught code prints to stdout.
   GE-SRC-RESET
   s" variable GECA   variable GECU" GE-SRC-LINE
   s" : GEC-GO ( -- ) GECA @ GECU @ INCLUDE-EVALUATE ;" GE-SRC-LINE
   s" : GEC-CATCH ( ptr u8 n -- n ) GECU ! GECA ! [: GEC-GO ;] catch ;" GE-SRC-LINE ;

: GE-EVAL-CATCH-RUN ( ptr u8 n -- ) {: src:ptr srcu:n :}
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   src srcu GE-SRC-S"
   s"  GEC-CATCH . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER ;

: GE-EVAL-INTERP-UNDEF-CATCH ( -- )
   \ Dot habu-interpret-err-under-8876b500: an undefined INTERPRET-mode token
   \ inside [: INCLUDE-EVALUATE ;] catch delivers the catchable RC-REJECT (70)
   \ of the rc-70 load-path contract — never a swallowed 0.
   s" qwertyuiop" GE-EVAL-CATCH-RUN
   s" hb eval interp-undef catch rc" GE-EXPECT-OK
   s" 70" s" hb eval interp-undef catch code" GE-EXPECT-OUT-HAS
   s" E-UNDEFINED" s" hb eval interp-undef catch diag" GE-EXPECT-ERR-HAS
   s" qwertyuiop" s" hb eval interp-undef catch token" GE-EXPECT-ERR-HAS
   s" PASS: interpret undefined under catch+evaluate -> caught 70" type cr ;

: GE-EVAL-UNDERFLOW-CATCH ( -- )
   \ Dot 8876b500 residual: interpret-level UNDERFLOW inside the same wrapper
   \ was the one interpret failure still rolling the eval frame back with only
   \ EVALERR set — catch read 0 (fail-open). It must be caught 70 exactly like
   \ E-UNDEFINED, with the sentinel stack under the wrapper intact.
   s" drop drop drop" GE-EVAL-CATCH-RUN
   s" hb eval underflow catch rc" GE-EXPECT-OK
   s" 70" s" hb eval underflow catch code" GE-EXPECT-OUT-HAS
   s" E-UNDERFLOW" s" hb eval underflow catch diag" GE-EXPECT-ERR-HAS
   s" drop" s" hb eval underflow catch token" GE-EXPECT-ERR-HAS
   s" PASS: interpret underflow under catch+evaluate -> caught 70" type cr ;

: GE-EVAL-UNDERFLOW-FAILCLOSED ( -- )
   \ No handler: the underflow throw escapes the eval frame and fails closed rc
   \ 70 (uncaught-throw exit), never continuing past the failed evaluate. The
   \ rollback-and-return path printed the marker and exited 0 (fail-open).
   GE-HB-RESET
   GE-SRC-RESET
   s" drop drop drop" GE-SRC-S"
   s"  INCLUDE-EVALUATE" GE-SRC-LINE
   s" s" GE-SRC+ GE-DQ GE-SRC-C s"  ALIVE-AFTER" GE-SRC+ GE-DQ GE-SRC-C
   s"  type cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb eval underflow no-catch rc" GE-EXPECT-RC
   s" E-UNDERFLOW" s" hb eval underflow no-catch diag" GE-EXPECT-ERR-HAS
   s" " s" hb eval underflow no-catch dead marker" GE-EXPECT-OUT
   s" PASS: interpret underflow in evaluate w/o catch -> fail-closed rc70" type cr ;

: GE-UNDERFLOW-TOPLEVEL-UNCHANGED ( -- )
   \ Plain-stdin contract pin for the fix: top-level underflow (EVALD==0) keeps
   \ the E-UNDERFLOW diagnostic + rc 70 exactly (GE-UNDERFLOW-DIAG shape).
   GE-HB-RESET
   GE-SRC-RESET
   s" drop drop drop" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb top-level underflow rc unchanged" GE-EXPECT-RC
   s" E-UNDERFLOW" s" hb top-level underflow diag unchanged" GE-EXPECT-ERR-HAS
   s" PASS: top-level underflow fail-closed rc70 (unchanged)" type cr ;

: GE-EVAL-INTERP-ERR-RECOVER ( -- )
   GE-EVAL-INTERP-UNDEF-CATCH
   GE-EVAL-UNDERFLOW-CATCH
   GE-EVAL-UNDERFLOW-FAILCLOSED
   GE-UNDERFLOW-TOPLEVEL-UNCHANGED ;

: GE-EVAL-DEF-REJECT-1 ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n diag:ptr diagu:n label:ptr labelu:n :}
   \ One rejected definition evaluated under the TCE-CATCH wrapper: the abort
   \ must deliver caught RC-REJECT (70) on stdout with the diagnostic on
   \ stderr and the process exiting 0 — never a SIGBUS register dump (rc 134).
   src srcu GE-EVAL-CATCH-RUN
   label labelu GE-EXPECT-OK
   s" 70" label labelu GE-EXPECT-OUT-HAS
   diag diagu label labelu GE-EXPECT-ERR-HAS ;

: GE-EVAL-DEF-REJECT-CATCH ( -- )
   \ Dot habu-def-compile-failure-7182eeb2 lock-in: a definition whose engine
   \ compile fails inside [: INCLUDE-EVALUATE ;] catch is a catchable throw
   \ with the eval frame rolled back. Exact dot repro (undefined in a :-body,
   \ formerly a habu-crash regs dump) plus the orderly reject battery — every
   \ shape that fail-closes rc 70 on plain stdin must be caught 70 here.
   s" : XG1 ( -- ) qwertyuiop ;" s" E-UNDEFINED"
      s" hb eval def-undef catch" GE-EVAL-DEF-REJECT-1
   s" : GDR1 ( -- ) drop ;" s" non-certified definition: gdr1"
      s" hb eval def-underdepth catch" GE-EVAL-DEF-REJECT-1
   s" : GDR2 ( n -- ) ;" s" non-certified definition: gdr2"
      s" hb eval def-unconsumed-in catch" GE-EVAL-DEF-REJECT-1
   s" : GDR3 ( -- n ) ;" s" non-certified definition: gdr3"
      s" hb eval def-missing-out catch" GE-EVAL-DEF-REJECT-1
   s" : GDR4 ( -- ) 1 2 ;" s" non-certified definition: gdr4"
      s" hb eval def-surplus-out catch" GE-EVAL-DEF-REJECT-1
   s" PASS: def-compile failures under catch+evaluate -> caught 70" type cr ;

: GE-ORPHAN-CLOSER-1 ( ptr u8 n ptr u8 n -- ) {: tok:ptr toku:n label:ptr labelu:n :}
   \ Plain stdin: a definition that opens no control-flow frame but names a closer
   \ must fail closed rc 70 with the named engine diagnostic + the offending token,
   \ NEVER a SIGBUS register dump (rc 134). Root cause (dot habu-orphan-control-
   \ word-0370b49d): every closer's compile-time patch pops the control-flow stack
   \ through LCFPOP; with an empty stack it underflowed to a bogus branch origin that
   \ LPAT then dereferenced. LCFPOP now guards depth 0 and rejects like E-UNDEFINED.
   GE-HB-RESET
   GE-SRC-RESET
   s" : XI ( -- ) " GE-SRC+
   tok toku GE-SRC+
   s"  ;" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 label labelu GE-EXPECT-RC
   s" control-flow closer without opener" label labelu GE-EXPECT-ERR-HAS
   tok toku label labelu GE-EXPECT-ERR-HAS
   s" habu-crash" label labelu GE-EXPECT-ERR-LACKS ;

: GE-ORPHAN-CLOSER ( -- )
   \ Every control-flow closer, orphaned at top level, and the catchable-under-eval
   \ subset. THEN/ELSE/REPEAT/LOOP/+LOOP/ENDOF crashed rc 134 before the fix;
   \ UNTIL/AGAIN/ENDCASE were orderly by luck (no LPAT deref / zeroed slack cell)
   \ but now share the one guarded LCFPOP reject path.
   s" THEN"    s" hb orphan then"    GE-ORPHAN-CLOSER-1
   s" ELSE"    s" hb orphan else"    GE-ORPHAN-CLOSER-1
   s" REPEAT"  s" hb orphan repeat"  GE-ORPHAN-CLOSER-1
   s" UNTIL"   s" hb orphan until"   GE-ORPHAN-CLOSER-1
   s" AGAIN"   s" hb orphan again"   GE-ORPHAN-CLOSER-1
   s" LOOP"    s" hb orphan loop"    GE-ORPHAN-CLOSER-1
   s" +LOOP"   s" hb orphan +loop"   GE-ORPHAN-CLOSER-1
   s" ENDOF"   s" hb orphan endof"   GE-ORPHAN-CLOSER-1
   s" ENDCASE" s" hb orphan endcase" GE-ORPHAN-CLOSER-1
   \ Under [: INCLUDE-EVALUATE ;] catch: the former SIGBUS closers deliver a
   \ catchable RC-REJECT (70), the eval frame rolled back, process exits 0.
   s" : XO1 ( -- ) THEN ;" s" control-flow closer without opener"
      s" hb eval orphan-then catch" GE-EVAL-DEF-REJECT-1
   s" : XO2 ( -- ) LOOP ;" s" control-flow closer without opener"
      s" hb eval orphan-loop catch" GE-EVAL-DEF-REJECT-1
   s" : XO3 ( -- ) REPEAT ;" s" control-flow closer without opener"
      s" hb eval orphan-repeat catch" GE-EVAL-DEF-REJECT-1
   s" PASS: orphan control-flow closers fail closed rc70 (no SIGBUS)" type cr ;

: GE-SET-CHECK-NEG ( -- )
   \ set-check is fail-closed at install (dot habu-stdlib-check-hook-fd883aea): a
   \ non-zero argument outside the live JIT code window [DBASE, CP) dies with a
   \ NAMED rc-70 diagnostic instead of BLRing into garbage at the next publish.
   \ 1 (below DBASE) and `dbase@ HOOK-CELL + @` (a code word mis-read from the
   \ wrong CODE base) are the two RCA shapes; both must exit 70, never signal.
   GE-HB-RESET
   GE-SRC-RESET s" 1 set-check" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb set-check tiny-xt rc" GE-EXPECT-RC
   s" set-check: invalid checker xt" s" hb set-check tiny-xt diag" GE-EXPECT-ERR-HAS
   GE-HB-RESET
   GE-SRC-RESET s" dbase@ $1B0 + @ set-check" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb set-check dbase-garbage rc" GE-EXPECT-RC
   s" set-check: invalid checker xt" s" hb set-check dbase-garbage diag" GE-EXPECT-ERR-HAS
   s" PASS: set-check fail-closed on garbage xt (rc 70, named diagnostic)" type cr ;

create GE-CF-BODY GE-SRC-CAP allot
variable GE-CF-BODY-U

: GE-CF-BODY-RESET ( -- )
   0 GE-CF-BODY-U ! ;

: GE-CF-BODY+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   GE-CF-BODY-U @ u + GE-SRC-CAP > if E-STR-CAPACITY throw then
   a GE-CF-BODY GE-CF-BODY-U @ + u BYTE-COPY
   GE-CF-BODY-U @ u + GE-CF-BODY-U ! ;

: GE-CF-BODY$ ( -- ptr u8 n )
   GE-CF-BODY GE-CF-BODY-U @ ;

\ Build ": DEEP ( -- ) " + n * "0 0= if " + n * "then " + " ;" — n balanced,
\ nested IF/THEN openers, each fed a real bool so a checkable depth certifies.
: GE-CF-NEST ( n -- ) {: n:n :}
   GE-CF-BODY-RESET
   s" : DEEP ( -- ) " GE-CF-BODY+
   n 0 ?do s" 0 0= if " GE-CF-BODY+ loop
   n 0 ?do s" then " GE-CF-BODY+ loop
   s"  ;" GE-CF-BODY+ ;

: GE-CF-OVERCAP-1 ( n ptr u8 n -- ) {: n:n label:ptr labelu:n :}
   \ Plain stdin: a definition nesting control flow past CFSTK-DEPTH-MAX must fail
   \ closed rc 70 with the named engine diagnostic + the offending opener token,
   \ NEVER a SIGABRT/SIGSEGV register dump. Root cause (dot habu-cap-native-
   \ control-a5669829): LCFPUSH had no overflow cap, so the depth-(cap) record
   \ spilled past [CFSTK-OFF, DICT-SIZE) into the JIT code area above it — the
   \ opposite-direction sibling of the LCFPOP orphan-underflow crash. LCFPUSH now
   \ guards depth == CFSTK-DEPTH-MAX and rejects like the orphan closer.
   GE-HB-RESET
   n GE-CF-NEST
   GE-CF-BODY$ RUNTIME-RUNNER:SOURCE
   70 label labelu GE-EXPECT-RC
   s" control-flow nesting too deep" label labelu GE-EXPECT-ERR-HAS
   s" if" label labelu GE-EXPECT-ERR-HAS
   s" habu-crash" label labelu GE-EXPECT-ERR-LACKS ;

: GE-CF-DEPTH-CAP ( -- )
   \ The over-cap battery. cap+1 is the exact overflow edge; a former hard-crash
   \ depth well past it both fail closed rc 70 with the diagnostic and no register
   \ dump. The region-full depth (exactly CFSTK-DEPTH-MAX records fit) is the
   \ checker's non-certified reject, NOT a cap reject — proving the native cap is
   \ the region edge. The checker's max checkable depth still compiles rc 0. The
   \ over-cap reject is catchable under evaluate (RC-REJECT 70 via LEVALREC).
   CFSTK-DEPTH-MAX 1 +  s" hb cf-cap plus1"      GE-CF-OVERCAP-1
   CFSTK-DEPTH-MAX 50 + s" hb cf-cap was-crash"  GE-CF-OVERCAP-1
   GE-HB-RESET
   CFSTK-DEPTH-MAX GE-CF-NEST
   GE-CF-BODY$ RUNTIME-RUNNER:SOURCE
   70 s" hb cf-cap region-full rc" GE-EXPECT-RC
   s" control-flow nesting too deep" s" hb cf-cap region-full not-cap" GE-EXPECT-ERR-LACKS
   s" habu-crash" s" hb cf-cap region-full no-crash" GE-EXPECT-ERR-LACKS
   GE-HB-RESET
   31 GE-CF-NEST
   GE-CF-BODY$ RUNTIME-RUNNER:SOURCE
   s" hb cf-cap legal-31" GE-EXPECT-OK
   CFSTK-DEPTH-MAX 1 + GE-CF-NEST
   GE-CF-BODY$ GE-EVAL-CATCH-RUN
   s" hb cf-cap eval-catch rc" GE-EXPECT-OK
   s" 70" s" hb cf-cap eval-catch code" GE-EXPECT-OUT-HAS
   s" control-flow nesting too deep" s" hb cf-cap eval-catch diag" GE-EXPECT-ERR-HAS
   s" PASS: control-flow depth cap fail-closed rc70 (no overflow, catchable)" type cr ;

: GE-RXE-CATCH-USABLE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n code:ptr codeu:n diag:ptr diagu:n :}
   \ dot habu-raw-exit-compile: one recoverable compile-error misuse evaluated
   \ under the TCE-CATCH wrapper. The die site (C-DUP-DEF-FAIL / C-PACKAGE-FAIL /
   \ C-LBRACE-DIE / C-LOCAL-REF / C-DIE-DOES) used to NR-EXIT-GROUP; it now writes
   \ its diagnostic to fd 2 and routes through LCOMPILEDIE, so inside evaluate the
   \ aborted compile is a catchable throw of its sysexits code. The caught code
   \ prints to stdout, then a FRESH definition (GEC-RXOK -> 12321) compiles and
   \ runs, proving the eval-frame rollback (input cursor + CP/NDICT truncation,
   \ HIDX skipping the stale rolled-back records) left a usable session. Exit 0,
   \ never a SIGBUS/register dump.
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   src srcu GE-SRC-S"
   s"  GEC-CATCH . cr" GE-SRC-LINE
   s" : GEC-RXOK ( -- n ) 12321 ; GEC-RXOK . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb rawexit eval-catch usable" GE-EXPECT-OK
   code codeu s" hb rawexit eval-catch code" GE-EXPECT-OUT-HAS
   s" 12321" s" hb rawexit eval-catch session-usable" GE-EXPECT-OUT-HAS
   diag diagu s" hb rawexit eval-catch diag" GE-EXPECT-ERR-HAS
   s" habu-crash" s" hb rawexit eval-catch no-crash" GE-EXPECT-ERR-LACKS ;

: GE-RXE-TOP ( ptr u8 n n ptr u8 n -- )
   {: src:ptr srcu:n rc:n diag:ptr diagu:n :}
   \ Same misuse at top level (EVALD==0, no eval frame): the recovery route only
   \ ADDS the inside-evaluate catch, so the fail-closed sysexits exit + diagnostic
   \ stay byte-identical to before the conversion.
   GE-HB-RESET
   GE-SRC-RESET
   src srcu GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   rc s" hb rawexit top-level rc" GE-EXPECT-RC
   diag diagu s" hb rawexit top-level diag" GE-EXPECT-ERR-HAS
   s" habu-crash" s" hb rawexit top-level no-crash" GE-EXPECT-ERR-LACKS ;

: GE-RAWEXIT-RECOVER ( -- )
   \ dot habu-raw-exit-compile: runtime-compiler die sites that used to
   \ NR-EXIT-GROUP now route through the shared LCOMPILEDIE tail — recoverable as
   \ a catchable throw of their sysexits code inside evaluate (eval frame rolled
   \ back, session usable), byte-identical fail-closed exit + diagnostic at top
   \ level. One representative misuse per converted family. Dict/code overflow
   \ (76/77) share the SAME LCOMPILEDIE tail; their top-level 77 contract is gated
   \ by GE-DICT-FULL, and the dup-def case here exercises the identical
   \ rollback-past-a-published-record + HIDX stale-tolerance path at unit cost.
   s" : RXF ( -- n ) 1 ; : RXF ( -- n ) 2 ;" s" 78" s" duplicate definition:" GE-RXE-CATCH-USABLE
   s" : RXF ( -- n ) 1 ; : RXF ( -- n ) 2 ;" 78 s" duplicate definition:" GE-RXE-TOP
   s" public" s" 75" s" public" GE-RXE-CATCH-USABLE
   s" public" 75 s" public" GE-RXE-TOP
   s" : RXQLB ( -- ) [: {: a :} a ;] drop ;" s" 75" s" local cannot be inside quotation" GE-RXE-CATCH-USABLE
   s" : RXQLB ( -- ) [: {: a :} a ;] drop ;" 75 s" local cannot be inside quotation" GE-RXE-TOP
   s" : RXQLR ( n -- ) {: myloc :} [: myloc drop ;] drop ;" s" 75" s" myloc" GE-RXE-CATCH-USABLE
   s" : RXQLR ( n -- ) {: myloc :} [: myloc drop ;] drop ;" 75 s" myloc" GE-RXE-TOP
   s" : RXMK ( -- ) create does> ( -- n ) ;" s" 70" s" does>" GE-RXE-CATCH-USABLE
   s" : RXMK ( -- ) create does> ( -- n ) ;" 70 s" does>" GE-RXE-TOP
   s" PASS: recoverable compile errors catchable inside evaluate (session usable) + fail-closed at top level" type cr ;

\ --- dot habu-convert-residual-compile-f460b9f2: residual compile-die conversions ---
\ The out-of-inventory recoverable die sites (J-DOES/J-QUOT/J-SEMIQUOT 75,
\ C-SIG-BAD 76, C-DEFER-DIE-TOKEN, C-QUOTE-EOF 74, counted-string 76,
\ C-LBRACE-STORE-ONE 75, postpone/export-undefined 70) now route through the same
\ LCOMPILEDIE tail: catchable inside evaluate, byte-identical fail-closed at top level.

create GE-RXE-TML-BUF 512 allot   variable GE-RXE-TML-U

: GE-RXE-TML-BUILD ( -- )   \ ": RXTML ( -- ) {: t0 t1 ... t64 :} ;" (65 locals: one over the 64 cap) into GE-RXE-TML-BUF via the GE-SRC scratch
   GE-SRC-RESET
   s" : RXTML ( -- ) {:" GE-SRC+
   65 0 ?do  GE-SRC-SP  s" t" GE-SRC+  i GE-SRC-U+  loop
   s"  :} ;" GE-SRC+
   GE-SRC-U @ GE-RXE-TML-U !
   GE-SRC-BUF GE-RXE-TML-BUF GE-RXE-TML-U @ BYTE-COPY ;

: GE-RXE-TML$ ( -- ptr u8 n )  GE-RXE-TML-BUF GE-RXE-TML-U @ ;

: GE-RXE-BS ( -- )  $5C GE-SRC-C ;                     \ backslash byte into the source builder

: GE-RXE-ESC-OPEN ( -- )                               \ append the `s\" ` escaped-string opener + delimiter space
   [char] s GE-SRC-C  GE-RXE-BS  GE-DQ GE-SRC-C  GE-SRC-SP ;

\ counted-string >255 (C-ICQ/C-EICQ/C-CQ/C-ECQ). This cap now carries a named fd-2
\ label ("hb: counted string too long (max 255)", dot habu-recovery-pkg-scope-e0bd98e2)
\ that disambiguates the 76 it shares with C-SIG-BAD; the assertions add that label on
\ both the eval-catch and top-level legs. The evaluate target `c" <256 A>"` embeds a "
\ so it is passed through an s\" wrapper with \q-escaped quotes.
: GE-RXE-CSTR-CATCH ( -- )
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   GE-RXE-ESC-OPEN                                      \ s\"
   [char] c GE-SRC-C  GE-RXE-BS  [char] q GE-SRC-C  GE-SRC-SP   \ c\q (-> c" ) + delimiter
   256 [char] A GE-SRC-REPEAT-C
   GE-RXE-BS  [char] q GE-SRC-C  GE-DQ GE-SRC-C         \ \q closes the counted string; " closes the s\" wrapper
   s"  GEC-CATCH . cr" GE-SRC-LINE
   s" : GEC-RXOK ( -- n ) 12321 ; GEC-RXOK . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb rxe cstr eval-catch usable" GE-EXPECT-OK
   s" 76" s" hb rxe cstr eval-catch code" GE-EXPECT-OUT-HAS
   s" 12321" s" hb rxe cstr eval-catch session-usable" GE-EXPECT-OUT-HAS
   s" counted string too long" s" hb rxe cstr eval-catch label" GE-EXPECT-ERR-HAS
   s" habu-crash" s" hb rxe cstr eval-catch no-crash" GE-EXPECT-ERR-LACKS ;

: GE-RXE-CSTR-TOP ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   [char] c GE-SRC-C  GE-DQ GE-SRC-C  GE-SRC-SP         \ c" + delimiter
   256 [char] A GE-SRC-REPEAT-C
   GE-DQ GE-SRC-C  GE-SRC-LF
   RUNTIME-RUNNER:BUFFER
   76 s" hb rxe cstr top rc" GE-EXPECT-RC
   s" counted string too long" s" hb rxe cstr top label" GE-EXPECT-ERR-HAS
   s" habu-crash" s" hb rxe cstr top no-crash" GE-EXPECT-ERR-LACKS ;

\ unterminated string literal (C-QUOTE-EOF). The evaluate target `s" abc` (no
\ closing quote) is s\"-wrapped so its embedded " does not close the wrapper.
: GE-RXE-QEOF-CATCH ( -- )
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   GE-RXE-ESC-OPEN                                      \ s\"
   [char] s GE-SRC-C  GE-RXE-BS  [char] q GE-SRC-C  GE-SRC-SP   \ s\q (-> s" ) + delimiter
   s" abc" GE-SRC+  GE-DQ GE-SRC-C                      \ abc" : the target `s" abc` is unterminated; " closes the wrapper
   s"  GEC-CATCH . cr" GE-SRC-LINE
   s" : GEC-RXOK ( -- n ) 12321 ; GEC-RXOK . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb rxe qeof eval-catch usable" GE-EXPECT-OK
   s" 74" s" hb rxe qeof eval-catch code" GE-EXPECT-OUT-HAS
   s" 12321" s" hb rxe qeof eval-catch session-usable" GE-EXPECT-OUT-HAS
   s" bad string literal" s" hb rxe qeof eval-catch diag" GE-EXPECT-ERR-HAS
   s" habu-crash" s" hb rxe qeof eval-catch no-crash" GE-EXPECT-ERR-LACKS ;

: GE-RXE-QEOF-TOP ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   [char] s GE-SRC-C  GE-DQ GE-SRC-C  GE-SRC-SP  s" abc" GE-SRC+  GE-SRC-LF   \ `s" abc` unterminated
   RUNTIME-RUNNER:BUFFER
   74 s" hb rxe qeof top rc" GE-EXPECT-RC
   s" bad string literal" s" hb rxe qeof top diag" GE-EXPECT-ERR-HAS
   s" habu-crash" s" hb rxe qeof top no-crash" GE-EXPECT-ERR-LACKS ;

: GE-RAWEXIT-RESIDUAL ( -- )
   \ One caught-inside-evaluate + top-level pair per converted site. The eval-catch
   \ leg proves catchable code + usable session (GEC-RXOK -> 12321); the top leg
   \ proves byte-identical fail-closed exit + diagnostic.
   s" : RXDOES ( -- ) create 1 {: v :} does> ( -- ) ;" s" 75" s" does>" GE-RXE-CATCH-USABLE
   s" : RXDOES ( -- ) create 1 {: v :} does> ( -- ) ;" 75 s" does>" GE-RXE-TOP
   s" : RXQ ( -- ) [: [: 5 ;] drop ;] drop ;" s" 75" s" [:" GE-RXE-CATCH-USABLE
   s" : RXQ ( -- ) [: [: 5 ;] drop ;] drop ;" 75 s" [:" GE-RXE-TOP
   s" : RXSQ ( -- ) 5 ;] drop ;" s" 75" s" ;]" GE-RXE-CATCH-USABLE
   s" : RXSQ ( -- ) 5 ;] drop ;" 75 s" ;]" GE-RXE-TOP
   s" defer RXDFR badsig" s" 76" s" RXDFR" GE-RXE-CATCH-USABLE
   s" defer RXDFR badsig" 76 s" RXDFR" GE-RXE-TOP
   s" defer" s" 74" s" defer" GE-RXE-CATCH-USABLE
   s" defer" 74 s" defer" GE-RXE-TOP
   s" : RXPP ( -- ) postpone RXNOPEWORD ;" s" 70" s" RXNOPEWORD" GE-RXE-CATCH-USABLE
   s" : RXPP ( -- ) postpone RXNOPEWORD ;" 70 s" RXNOPEWORD" GE-RXE-TOP
   s" package RXPKG public export RXNOEXPORT ;package" s" 70" s" RXNOEXPORT" GE-RXE-CATCH-USABLE
   s" package RXPKG public export RXNOEXPORT ;package" 70 s" RXNOEXPORT" GE-RXE-TOP
   GE-RXE-TML-BUILD
   GE-RXE-TML$ s" 75" s" t64" GE-RXE-CATCH-USABLE
   GE-RXE-TML$ 75 s" t64" GE-RXE-TOP
   GE-RXE-QEOF-CATCH
   GE-RXE-QEOF-TOP
   GE-RXE-CSTR-CATCH
   GE-RXE-CSTR-TOP
   s" PASS: residual compile dies recover inside evaluate + fail-closed at top level" type cr ;

\ --- Package-scope rollback across compile-error recovery (dot habu-recovery-pkg-scope-e0bd98e2) ---
\ A compile error aborting an in-package definition must roll the OPEN-PACKAGE scope
\ back to the boundary scope, exactly like CP/NDICT/DP. Before the fix the package
\ stayed dangling open and later top-level defines landed in it silently.

: GE-PKGSCOPE-EVAL-CLOSED ( -- )
   \ Package opened INSIDE the failing evaluate: caught, and the eval-frame recovery
   \ restores the evaluate-entry scope (global), so the package is CLOSED. Discriminator:
   \ a dangling package would make the following top-level `package` nest-fail (exit 75
   \ mid-batch, so GEC-RXOK never prints); a restored global scope opens+closes it and
   \ reaches the fresh global define.
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   s" package PKX public export PKNOPE ;package" GE-SRC-S"
   s"  GEC-CATCH . cr" GE-SRC-LINE
   s" package PKY ;package" GE-SRC-LINE
   s" : GEC-RXOK ( -- n ) 12321 ; GEC-RXOK . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb pkgscope eval-closed rc" GE-EXPECT-OK
   s" 70" s" hb pkgscope eval-closed code" GE-EXPECT-OUT-HAS
   s" 12321" s" hb pkgscope eval-closed scope-restored-global" GE-EXPECT-OUT-HAS
   s" habu-crash" s" hb pkgscope eval-closed no-crash" GE-EXPECT-ERR-LACKS ;

: GE-PKGSCOPE-CHECKER-RESYNC ( -- )
   \ With NO intervening package op between the caught error and a checked reference:
   \ the bare def GEC-W1 must record in GLOBAL checker scope, and the later checked
   \ GEC-W2 (after a real package op that would orphan a mis-recorded PKX:GEC-W1) must
   \ still resolve. An engine-only rollback (checker left stale-PKX) rc70s at GEC-W2.
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   s" package PKX public export PKNOPE ;package" GE-SRC-S"
   s"  GEC-CATCH . cr" GE-SRC-LINE
   s" : GEC-W1 ( -- n ) 321 ;" GE-SRC-LINE
   s" package PKZ ;package" GE-SRC-LINE
   s" : GEC-W2 ( -- n ) GEC-W1 ; GEC-W2 . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb pkgscope checker-resync rc" GE-EXPECT-OK
   s" 321" s" hb pkgscope checker-resync resolves-global" GE-EXPECT-OUT-HAS ;

: GE-PKGSCOPE-EVAL-STAYS ( -- )
   \ Package legitimately open at evaluate ENTRY must NOT be closed by recovery: the
   \ eval-frame rollback restores the evaluate-entry scope. AA open at top level; the
   \ failing string does not touch packages, so after the caught error AA is still open
   \ (AA:BEFOREW / AA:AFTERW both resolve public) and `;package` closes it cleanly.
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   s" package AA public" GE-SRC-LINE
   s" : BEFOREW ( -- n ) 11 ;" GE-SRC-LINE
   s" : FOO ( -- ) NOPEWORD ;" GE-SRC-S"
   s"  GEC-CATCH . cr" GE-SRC-LINE
   s" : AFTERW ( -- n ) 22 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" AA:BEFOREW AA:AFTERW + . cr" GE-SRC-LINE     \ 11+22=33 proves both landed in AA:public (`.` is newline-terminated here)
   RUNTIME-RUNNER:BUFFER
   s" hb pkgscope eval-stays rc" GE-EXPECT-OK
   s" 70" s" hb pkgscope eval-stays code" GE-EXPECT-OUT-HAS
   s" 33" s" hb pkgscope eval-stays package-stays-open" GE-EXPECT-OUT-HAS ;

: GE-PKGSCOPE-TOP-EXIT ( -- )
   \ Top-level (EVALD==0, no eval frame, no handler): an in-package compile error is
   \ still a fail-closed exit — package-scope rollback does not change the exit path.
   GE-HB-RESET
   GE-SRC-RESET
   s" package PKT public export PKTNOPE ;package" GE-SRC-LINE
   s" : NEVER ( -- n ) 999 ; NEVER . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   \ process exits at the export error before NEVER/999 (fail-closed)
   70 s" hb pkgscope top-exit fail-closed rc" GE-EXPECT-RC ;

: GE-PKGSCOPE-RECOVERY ( -- )
   GE-PKGSCOPE-EVAL-CLOSED
   GE-PKGSCOPE-CHECKER-RESYNC
   GE-PKGSCOPE-EVAL-STAYS
   GE-PKGSCOPE-TOP-EXIT
   s" PASS: package scope rolls back on compile-error recovery (closed/stays-open/checker/top-exit)" type cr ;

package RUNTIME-CHECKS
public

: REST ( -- )
   GENG:ASSERT-RUNTIME-SUBJECT
   GE-UNCAUGHT-THROW
   GE-INTERP-LAYOUT
   GE-MATCH-EXEC
   GE-DICT-FULL
   GE-DATA-FULL
   GE-DIV-MOD
   GE-PROCESS-PTY
   GE-TRUST-RUN
   GE-ARGV-MODES
   GE-UNDERFLOW-DIAG
   GE-DEREF-ARITY-DIAG
   GE-NESTED-CHECKED-DEF
   GE-NESTED-BAD-DEF
   GE-EVAL-UNDEF-RECOVER
   GE-EVAL-INTERP-ERR-RECOVER
   GE-EVAL-DEF-REJECT-CATCH
   GE-ORPHAN-CLOSER
   GE-CF-DEPTH-CAP
   GE-RAWEXIT-RECOVER
   GE-RAWEXIT-RESIDUAL
   GE-PKGSCOPE-RECOVERY
   GE-SET-CHECK-NEG
   GE-TYPED-SMOKE
   GE-TIMEOUT-ATTRIBUTION ;

: ALL ( -- )
   GE-CONSTRUCT-EXEC
   REST ;

;package

: GE-RUNTIME-CHECKS ( -- )
   RUNTIME-CHECKS:ALL ;

: GENG-BUILD-SLICE ( -- )
   GE-BUILD-FIXPOINT
   GT-CLEANUP
   s" PASS: native engine build gate slice" type cr ;

: GE-CANDIDATE-VALIDATE ( -- )
   s" candidate-validate" GS-EVENT
   GE-CANDIDATE!
   GE-EXPECT-CANDIDATE
   GE-CANDIDATE-SIZE-CHECK
   GE-CANDIDATE$ GATE-VALIDATION:RUN ;

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

package RUNTIME-WORKER
private

\ Runtime-slice time ratchet budget.
\
\ This slice times two candidate-engine process spawns - the executable-identity
\ negative plus one candidate-runtime worker fork, and the worker itself forks a
\ nested SUBJECT tree. Until now it was pinned to a naked 10000 ms wall-clock
\ constant while every other process-spawning ratchet in the gate - the
\ stdlib tail ratchet (test/tail-ratchet.f TAIL-BUDGET:PROCESS-MS =
\ 10000 TEST-BUDGET:PERF-MS) and the whole-gate stop-lines (test/run-lib.f
\ TEST:CAL-SCALED) - already scales its nominal by the measured host-calibration
\ factor. That factor (lib/test/budget.f: a fixed-work integer spin measured
\ against an idle-box reference, clamped to [100..300]% and exported by the gate
\ as the host-calibration percentage) is the repo's canonical load signal. The
\ runtime slice being the LONE exception is the bug: on a box running several
\ gate lanes plus unrelated user work the fixed 10000 ms bar false-reds on
\ engines byte-identical to ones that passed it quiet (measured 2026-07-19:
\ 10047..11919 ms, rc 0, zero correctness failures), because the child process
\ tree contends for CPU while the bar does not move. Putting this slice on the
\ SAME calibration is the root-cause fix, not a workaround.
\
\ Why calibration scaling is load-aware yet still catches a regression, and why
\ this is a measured/bounded budget rather than a "pass under load" exemption:
\   - a slow BOX widens the fixed-work calibration spin, so BUDGET-MS widens
\     proportionally and the load-contention false red disappears;
\   - a slow ENGINE does NOT move the fixed-work spin, so the scaled budget stays
\     tight and OVER? still fires - at ANY load, because the [100..300]% clamp
\     bounds compensation to 3x, so an engine slower than 3x nominal reds even on
\     a fully saturated box (proven by RATCHET-SELFTEST case 3);
\   - the decision stays a hard `elapsed > BUDGET-MS` FAIL; calibration only sets
\     the budget, it never short-circuits the comparison.
\
\ NOMINAL-MS derivation (macOS arm64, measured 2026-07-19 on this host; the box
\ carried an ambient loadavg of ~5-6 from an Unreal cook and a zig test, so none
\ of these are truly idle):
\   - standalone timed body, near-baseline: 5693..5860 ms at calibration 111..120%;
\   - standalone under three competing fixpoint-build lanes (loadavg 18-19):
\     7636..8529 ms at calibration 114..131%;
\   - FULL native gate, normal operating load (loadavg 6-7): 10987 ms at
\     calibration 115% (matches the orchestrator's measured 10986 ms red);
\   - FULL native gate, heavy load (loadavg 16): 14621 ms at calibration 120%.
\ The full gate adds intra-gate phase concurrency the standalone harness cannot
\ reproduce, so its numbers are the ones that matter. The worst calibration-
\ normalized elapsed (budget must exceed elapsed*100/calibration for the scaled
\ budget to clear it) is 14621/1.20 = 12184 ms. Applying the spark cold-budget
\ precedent's +25% safety margin (commit 9d91057e / 76f5e652) gives 15230 ms,
\ rounded up to a clean 16000 ms stop-line. At the measured normal load this
\ leaves budget 16000*1.15 = 18400 ms against 10987 ms elapsed (1.67x margin);
\ at the heavy-load worst case 16000*1.20 = 19200 ms against 14621 ms (1.31x).
16000 constant NOMINAL-MS

variable START-NS

: WRONG-EXE$ ( -- ptr u8 n )
   s" /usr/bin/true" ;

: START ( -- )
   RUNTIME-DIRECT:RESET
   mono-ns START-NS ! ;

: BUDGET-MS ( -- n )                     \ nominal scaled by the measured host calibration
   NOMINAL-MS TEST-BUDGET:PERF-MS ;

: CAL-PCT ( -- n )                       \ live host-calibration factor, as a percentage
   100 TEST-BUDGET:PERF-MS ;

: SATURATED? ( -- bool )                 \ calibration pinned at the clamp: box slower than 3x
   CAL-PCT T-BUDGET-MAX-PCT >= ;

: OVER? ( n -- bool )                    \ hard ratchet decision against the live calibrated budget
   BUDGET-MS > ;

: REPORT ( n n -- ) {: elapsed:n budget:n :}
   s" runtime elapsed-ms=" type elapsed GT-U-TYPE
   s"  max-ms=" type budget GT-U-TYPE
   s"  cal-pct=" type CAL-PCT GT-U-TYPE
   SATURATED? if s"  (saturated)" type then cr ;

: CHECK-TIME ( -- )
   mono-ns START-NS @ - PROC-NS-PER-MS / {: elapsed:n :}
   elapsed BUDGET-MS REPORT
   elapsed OVER? if
      s" runtime time ratchet exceeded" GE-FAIL
   then ;

: EXPECT-OVER ( n ptr u8 n -- ) {: elapsed:n label:ptr labelu:n :}
   elapsed OVER? 0= if label labelu GE-FAIL then ;

: EXPECT-WITHIN ( n ptr u8 n -- ) {: elapsed:n label:ptr labelu:n :}
   elapsed OVER? if label labelu GE-FAIL then ;

: CHECK-EXEC ( -- )
   RUNTIME-DIRECT:EXEC# {: count:n :}
   count RUNTIME-DIRECT:OWNER-LIMIT > if
      s" runtime process-exec=" type count .
      s" max-exec=" type RUNTIME-DIRECT:OWNER-LIMIT . cr
      s" runtime process ratchet exceeded" GE-FAIL
   then ;

: IDENTITY-NEG ( -- )
   GE-HB-RESET
   s" HABU_UNDER_TEST" >LEN WRONG-EXE$ >LEN PROC-ENV+
   s" test/candidate-runtime.f" GE-ARG+
   RUNTIME-DIRECT:IDENTITY-WORKER
   GE-IDENTITY-RC s" candidate worker executable identity mismatch" GE-EXPECT-RC
   s" candidate runtime: executable identity mismatch"
      s" candidate worker executable identity mismatch" GE-EXPECT-ERR-HAS
   s" PASS: candidate worker executable identity fails closed" type cr ;

: RUN ( ptr u8 n -- ) {: mode:ptr modeu:n :}
   GE-HB$ {: exe:ptr exeu:n :}
   GE-HB-RESET
   s" HABU_UNDER_TEST" >LEN exe exeu >LEN PROC-ENV+
   s" test/candidate-runtime.f" GE-ARG+
   modeu 0 > if mode modeu GE-ARG+ then
   RUNTIME-DIRECT:WORKER
   s" candidate runtime worker" GE-EXPECT-OK
   GT-ERR$ nip 0 <> if s" candidate runtime worker stderr" GE-FAIL then
   GT-OUT$ type ;

public

\ Negative/property proof for the load-conditioned ratchet (dot
\ habu-derive-runtime-budget-81b2f538). Pins the calibration with PERF-SET so the
\ four cases are deterministic, then restores it. GE-FAIL dies (exit 1), so a
\ mismatch fails the slice closed exactly like a real ratchet breach. The cases
\ prove, together, that the ratchet still catches a genuinely slower engine at
\ ANY load while tolerating pure load inflation:
\   1. at calibration 100% (quiet box) an elapsed just over nominal reds;
\   2. at calibration 100% an elapsed just under nominal passes (no false red);
\   3. at calibration 300% (fully saturated, clamped) an elapsed over 3x nominal
\      STILL reds - the clamp bounds compensation so a >3x-nominal engine cannot
\      hide behind load;
\   4. at calibration 300% an elapsed of 2x nominal - which would red on a quiet
\      box - passes, i.e. measured load widens the budget and kills the false red.
: RATCHET-SELFTEST ( -- )
   100 TEST-BUDGET:PERF-MS {: saved:n :}       \ snapshot the live calibration
   100 TEST-BUDGET:PERF-SET
   NOMINAL-MS 1000 +
      s" runtime ratchet catches over-budget slice at calibration 100%" EXPECT-OVER
   NOMINAL-MS 1000 -
      s" runtime ratchet passes within-budget slice at calibration 100%" EXPECT-WITHIN
   300 TEST-BUDGET:PERF-SET
   NOMINAL-MS 3 * 1000 +
      s" runtime ratchet catches slower engine even at max calibration" EXPECT-OVER
   NOMINAL-MS 2 *
      s" runtime ratchet tolerates load-inflated elapsed at max calibration" EXPECT-WITHIN
   saved TEST-BUDGET:PERF-SET                   \ restore the live calibration
   s" PASS: runtime ratchet load-conditioned decision (regression caught at any load)" type cr ;

: SUBJECT ( -- )
   START
   IDENTITY-NEG
   s" " RUN
   CHECK-TIME
   CHECK-EXEC
   RATCHET-SELFTEST
   s" PASS: runtime process/time ratchet" type cr ;

: PARITY ( -- )
   IDENTITY-NEG
   s" runtime-parity" RUN ;

: CONSTRUCT ( -- )
   s" construct-parity" RUN ;

;package

: GENG-RUNTIME-SLICE ( -- )
   s" hb-gate-engine-runtime" GT-START
   RUNTIME-WORKER:SUBJECT
   GT-CLEANUP
   s" PASS: native engine runtime gate slice" type cr ;

: GENG-RUNTIME-PARITY-SLICE ( -- )
   s" hb-gate-runtime-parity" GT-START
   RUNTIME-WORKER:PARITY
   GT-CLEANUP
   s" PASS: exact candidate runtime direct/subject parity slice" type cr ;

: GENG-CONSTRUCT-PARITY-SLICE ( -- )
   s" hb-gate-construct-parity" GT-START
   RUNTIME-WORKER:CONSTRUCT
   GT-CLEANUP
   s" PASS: exact candidate construct parity slice" type cr ;

: GE-MAIN ( -- )
   GENG-PARSE-SLICE
   GENG-SLICE @ GENG-BUILD-ID = if GENG-BUILD-SLICE exit then
   GENG-SLICE @ GENG-FIXTURES-ID = if GENG-FIXTURES-SLICE exit then
   GENG-SLICE @ GENG-REPAIR-ID = if GENG-REPAIR-SLICE exit then
   GENG-SLICE @ GENG-RUNTIME-ID = if GENG-RUNTIME-SLICE exit then
   GENG-SLICE @ GENG-RUNTIME-PARITY-ID = if GENG-RUNTIME-PARITY-SLICE exit then
   GENG-SLICE @ GENG-VALIDATE-ID = if GENG-VALIDATE-SLICE exit then
   GENG-SLICE @ GENG-CONSTRUCT-ID = if GENG-CONSTRUCT-PARITY-SLICE exit then
   GE-BUILD-FIXPOINT
   GE-RUN-EXTRA-FIXTURES
   GE-CANDIDATE-VALIDATE
   RUNTIME-WORKER:SUBJECT
   GT-CLEANUP
   s" PASS: native engine gate phase" type cr ;
