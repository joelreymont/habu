\ kernel-perf-lint-test.f - checked fixtures for the kernel profile-row diff lint.
\ Run: bin/hb tools/kernel-perf-lint-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/ptx/perf-registry.f
require tools/ptx/perf-watch.f
require tools/kernel-perf-lint-core.f

package KPL-TEST
private

4096 constant KLT-CAP
9 constant KLT-TAB

create KLT-OUT KLT-CAP allot
create KLT-FILE-BUF FS-PATH-CAP allot
create FILE2-BUF FS-PATH-CAP allot

variable KLT-OUT-U
variable KLT-FILE-U
variable FILE2-U

: TAB+ ( -- )
   KLT-TAB SB-APPEND-C ;

: LF+ ( -- )
   10 SB-APPEND-C ;

: KLT-HEAD+ ( ptr u8 n -- ) {: pa:ptr pu:n :}
   s" diff --git a/" SB-APPEND pa pu SB-APPEND
   s"  b/" SB-APPEND pa pu SB-APPEND LF+
   s" index 1234567..abcdef0 100644" SB-APPEND LF+
   s" --- a/" SB-APPEND pa pu SB-APPEND LF+
   s" +++ b/" SB-APPEND pa pu SB-APPEND LF+
   s" @@ -0,0 +1 @@" SB-APPEND LF+ ;

: KLT-ROW+ ( -- )   \ one valid added GBS registry row
   s" +KLT-KERNEL" SB-APPEND TAB+
   s" 4" SB-APPEND TAB+ s" 1" SB-APPEND TAB+
   s" 256" SB-APPEND TAB+ s" 1" SB-APPEND TAB+
   s" 10" SB-APPEND TAB+ s" 1024" SB-APPEND TAB+
   s" GBS" SB-APPEND TAB+ s" 42000" SB-APPEND TAB+
   s" test-dev" SB-APPEND TAB+ s" 2026-07-13" SB-APPEND TAB+
   s" lint fixture" SB-APPEND LF+ ;

: KLT-WAIVER+ ( -- )   \ one valid added WAIVER row owning the touched cg-matmul emitter
   s" +KLT-KERNEL" SB-APPEND TAB+
   s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" test-dev" SB-APPEND TAB+ s" 2026-07-13" SB-APPEND TAB+
   s" device-gated: fixture waiver" SB-APPEND TAB+
   s" lib/ptx/cg-matmul.f" SB-APPEND TAB+ s" 1" SB-APPEND LF+ ;

: WATCHED-NOROW$ ( -- ptr u8 n )
   SB-RESET
   s" lib/ptx/cg-matmul.f" KLT-HEAD+
   s" +\ tweak the tile inner loop" SB-APPEND LF+
   SB$ ;

: WATCHED+ROW$ ( -- ptr u8 n )
   SB-RESET
   s" lib/ptx/cg-matmul.f" KLT-HEAD+
   s" +\ tweak the tile inner loop" SB-APPEND LF+
   s" tools/ptx/perf-rows.tsv" KLT-HEAD+
   KLT-ROW+
   SB$ ;

: WATCHED+WAIVER$ ( -- ptr u8 n )
   SB-RESET
   s" lib/ptx/cg-matmul.f" KLT-HEAD+
   s" +\ tweak the tile inner loop" SB-APPEND LF+
   s" tools/ptx/perf-rows.tsv" KLT-HEAD+
   KLT-WAIVER+
   SB$ ;

: WATCHED+COMMENT$ ( -- ptr u8 n )   \ registry touched but only a comment added
   SB-RESET
   s" lib/ptx/cg-matmul.f" KLT-HEAD+
   s" +\ tweak the tile inner loop" SB-APPEND LF+
   s" tools/ptx/perf-rows.tsv" KLT-HEAD+
   s" +# just a comment" SB-APPEND LF+
   SB$ ;

: UNRELATED$ ( -- ptr u8 n )   \ a test near-miss of a watched producer stays clean
   SB-RESET
   s" lib/ptx/tile-test.f" KLT-HEAD+
   s" +\ tile DSL test tweak" SB-APPEND LF+
   SB$ ;

: TOOLSCG$ ( -- ptr u8 n )
   SB-RESET
   s" tools/ptx/saxpy-v4-cg.f" KLT-HEAD+
   s" +\ unroll one more chunk" SB-APPEND LF+
   SB$ ;

: EMIT$ ( -- ptr u8 n )
   SB-RESET
   s" src/arch/ptx/emit.f" KLT-HEAD+
   s" +\ encoder tweak" SB-APPEND LF+
   SB$ ;

: MODE$ ( ptr u8 n -- ptr u8 n ) {: path:ptr pathu:n :}
   SB-RESET
   s" diff --git a/" SB-APPEND path pathu SB-APPEND
   s"  b/" SB-APPEND path pathu SB-APPEND LF+
   s" old mode 100644" SB-APPEND LF+
   s" new mode 100755" SB-APPEND LF+
   SB$ ;

: TRUNC$ ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/a.f b/a.f" SB-APPEND LF+
   s" old mode 100644" SB-APPEND LF+
   s" new mode 100755" SB-APPEND
   SB$ ;

: SPOOF$ ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/a.f b/a.f" SB-APPEND LF+
   s" +++ b/spoof.f" SB-APPEND LF+
   SB$ ;

: CONTROL$ ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/tab" SB-APPEND KLT-TAB SB-APPEND-C
   s" x.f b/tab" SB-APPEND KLT-TAB SB-APPEND-C
   s" x.f" SB-APPEND LF+
   SB$ ;

: DELETED$ ( -- ptr u8 n )   \ deleting a kernel emitter is a perf-relevant change
   SB-RESET
   s" diff --git a/lib/ptx/cg-vec.f b/lib/ptx/cg-vec.f" SB-APPEND LF+
   s" deleted file mode 100644" SB-APPEND LF+
   s" index abcdef0..0000000" SB-APPEND LF+
   s" --- a/lib/ptx/cg-vec.f" SB-APPEND LF+
   s" +++ /dev/null" SB-APPEND LF+
   s" @@ -1 +0,0 @@" SB-APPEND LF+
   s" -\ gone" SB-APPEND LF+
   SB$ ;

: BADROW$ ( -- ptr u8 n )
   SB-RESET
   s" tools/ptx/perf-rows.tsv" KLT-HEAD+
   s" +KLT-KERNEL not-a-valid-row" SB-APPEND LF+
   SB$ ;

: KLT-RUN ( ptr u8 n -- n n ) {: a:ptr u:n :}   \ diff text -- out-len rc
   KERNEL-PERF-LINT:RESET
   KLT-OUT KLT-CAP LINT-OUT-BUFFER!
   a u KERNEL-PERF-LINT:SOURCE
   [: KERNEL-PERF-LINT:FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   rc ;

: KLT-CLEAN ( n n -- ) {: outu:n rc:n :}
   rc 0 T=
   outu 0 T= ;

: KLT-EXPECT ( n n ptr u8 n -- ) {: outu:n rc:n ma:ptr mu:n :}
   rc 1 T=
   KLT-OUT outu ma mu CONTAINS? TTRUE ;

: KLT-MISSING$ ( -- ptr u8 n )
   s" E-PERF-ROW-MISSING" ;

: TOUCH$ ( ptr u8 n -- ptr u8 n ) {: pa:ptr pu:n :}   \ diff touching one path, no registry row
   SB-RESET
   pa pu KLT-HEAD+
   s" +\ perf-relevant tweak" SB-APPEND LF+
   SB$ ;

: TOUCH-MISSING ( ptr u8 n -- )   \ a watched producer touched without a row must fail
   TOUCH$ KLT-RUN KLT-MISSING$ KLT-EXPECT ;

: TOUCH-CLEAN ( ptr u8 n -- )   \ a near-miss / non-producer path must stay clean
   TOUCH$ KLT-RUN KLT-CLEAN ;

: NEW-PRODUCER-TESTS ( -- )   \ every producer the fix newly names fails closed on a bare touch
   s" lib/ptx/tile.f" TOUCH-MISSING
   s" lib/ptx/tile-v4.f" TOUCH-MISSING
   s" lib/ptx/opt.f" TOUCH-MISSING
   s" lib/ptx/opt-ir.f" TOUCH-MISSING
   s" lib/ptx/ir.f" TOUCH-MISSING
   s" lib/ptx/collective.f" TOUCH-MISSING
   s" lib/ptx/cg-collective.f" TOUCH-MISSING ;

: NEAR-MISS-TESTS ( -- )   \ test near-misses and declared non-producers never trigger
   s" lib/ptx/tile-test.f" TOUCH-CLEAN
   s" lib/ptx/tile-v4-test.f" TOUCH-CLEAN
   s" lib/ptx/opt-test.f" TOUCH-CLEAN
   s" lib/ptx/opt-ir-test.f" TOUCH-CLEAN
   s" lib/ptx/ir-test.f" TOUCH-CLEAN
   s" lib/ptx/collective-test.f" TOUCH-CLEAN
   s" lib/ptx/cg-collective-test.f" TOUCH-CLEAN
   s" lib/ptx/cuda-driver.f" TOUCH-CLEAN
   s" lib/ptx/toolchain.f" TOUCH-CLEAN ;

: KLT-SOURCE-TESTS ( -- )
   WATCHED-NOROW$ KLT-RUN KLT-MISSING$ KLT-EXPECT
   TOOLSCG$ KLT-RUN KLT-MISSING$ KLT-EXPECT
   EMIT$ KLT-RUN KLT-MISSING$ KLT-EXPECT
   DELETED$ KLT-RUN KLT-MISSING$ KLT-EXPECT
   WATCHED+COMMENT$ KLT-RUN KLT-MISSING$ KLT-EXPECT
   BADROW$ KLT-RUN s" E-PERF-BAD-ROW" KLT-EXPECT
   WATCHED+ROW$ KLT-RUN KLT-CLEAN
   WATCHED+WAIVER$ KLT-RUN KLT-CLEAN
   UNRELATED$ KLT-RUN KLT-CLEAN ;

: KLT-FILE$ ( -- ptr u8 n )
   KLT-FILE-BUF KLT-FILE-U @ ;

: FILE2$ ( -- ptr u8 n )
   FILE2-BUF FILE2-U @ ;

: KLT-FILE-TESTS ( -- )   \ the file entrypoint reports the same finding
   CLEANUP-RESET
   s" habu-kernel-perf-lint" TMPDIR-MKDIR {: ra:ptr ru:n :}
   ra ru CLEANUP-DIR+
   ra ru s" watched.diff" KLT-FILE-BUF JOIN-PATH KLT-FILE-U !
   KLT-FILE$ CLEANUP+
   KLT-FILE$ WATCHED-NOROW$ WRITE-ALL
   KERNEL-PERF-LINT:RESET
   KLT-OUT KLT-CAP LINT-OUT-BUFFER!
   KLT-FILE$ KERNEL-PERF-LINT:FILE
   [: KERNEL-PERF-LINT:FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   rc KLT-MISSING$ KLT-EXPECT
   CLEANUP-RUN ;

: MULTI-FILE-TESTS ( -- )
   CLEANUP-RESET
   s" habu-kernel-perf-multi" TMPDIR-MKDIR {: ra:ptr ru:n :}
   ra ru CLEANUP-DIR+
   ra ru s" watched.diff" KLT-FILE-BUF JOIN-PATH KLT-FILE-U !
   ra ru s" other.diff" FILE2-BUF JOIN-PATH FILE2-U !
   KLT-FILE$ CLEANUP+ FILE2$ CLEANUP+
   KLT-FILE$ s" lib/ptx/opt.f" MODE$ WRITE-ALL
   FILE2$ s" lib/ptx/tile.f" MODE$ WRITE-ALL
   KERNEL-PERF-LINT:RESET
   KLT-OUT KLT-CAP LINT-OUT-BUFFER!
   KLT-FILE$ KERNEL-PERF-LINT:FILE
   FILE2$ KERNEL-PERF-LINT:FILE
   [: KERNEL-PERF-LINT:FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   rc KLT-MISSING$ KLT-EXPECT
   CLEANUP-RUN ;

: MALFORMED ( -- )
   KERNEL-PERF-LINT:RESET
   TRUNC$ KERNEL-PERF-LINT:SOURCE ;

: SPOOF ( -- )
   KERNEL-PERF-LINT:RESET
   SPOOF$ KERNEL-PERF-LINT:SOURCE ;

: CONTROL ( -- )
   KERNEL-PERF-LINT:RESET
   CONTROL$ KERNEL-PERF-LINT:SOURCE ;

: MALFORMED-TESTS ( -- )
   [: MALFORMED ;] E-DIFF-SYNTAX TTHROWSQ
   [: SPOOF ;] E-DIFF-SYNTAX TTHROWSQ
   [: CONTROL ;] E-DIFF-SYNTAX TTHROWSQ ;

\ ---- waiver-lifecycle ratchet ------------------------------------------------
\ The standing registry is a fixture file (REG-LOAD!); the diff drives the touch.

create REGFILE-BUF FS-PATH-CAP allot
variable REGFILE-U

: REGFILE$ ( -- ptr u8 n )
   REGFILE-BUF REGFILE-U @ ;

: DWAIVER+ ( ptr u8 n ptr u8 n -- ) {: ea:ptr eu va:ptr vu :}   \ diff-added waiver for KLT-KERNEL
   s" +KLT-KERNEL" SB-APPEND TAB+ s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" test-dev" SB-APPEND TAB+ s" 2026-07-13" SB-APPEND TAB+ s" fixture" SB-APPEND TAB+
   ea eu SB-APPEND TAB+ va vu SB-APPEND LF+ ;

: FWAIVER+ ( ptr u8 n ptr u8 n -- ) {: ea:ptr eu va:ptr vu :}   \ committed registry waiver line
   s" KLT-KERNEL" SB-APPEND TAB+ s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" test-dev" SB-APPEND TAB+ s" 2026-07-13" SB-APPEND TAB+ s" fixture" SB-APPEND TAB+
   ea eu SB-APPEND TAB+ va vu SB-APPEND LF+ ;

: TOUCH-MMA+ ( -- )
   s" lib/ptx/cg-mma.f" KLT-HEAD+
   s" +\ tweak the mma inner loop" SB-APPEND LF+ ;

: REG-HEAD+ ( -- )
   s" tools/ptx/perf-rows.tsv" KLT-HEAD+ ;

: R-NOROW$ ( -- ptr u8 n )        SB-RESET TOUCH-MMA+ SB$ ;
: R-WAIVER-V1$ ( -- ptr u8 n )    SB-RESET TOUCH-MMA+ REG-HEAD+ s" lib/ptx/cg-mma.f" s" 1" DWAIVER+ SB$ ;
: R-WAIVER-V2$ ( -- ptr u8 n )    SB-RESET TOUCH-MMA+ REG-HEAD+ s" lib/ptx/cg-mma.f" s" 2" DWAIVER+ SB$ ;
: R-WAIVER-OTHER$ ( -- ptr u8 n ) SB-RESET TOUCH-MMA+ REG-HEAD+ s" lib/ptx/cg-matmul.f" s" 1" DWAIVER+ SB$ ;
: R-MEAS$ ( -- ptr u8 n )         SB-RESET TOUCH-MMA+ REG-HEAD+ KLT-ROW+ SB$ ;
: R-UNKNOWN$ ( -- ptr u8 n )      SB-RESET TOUCH-MMA+ REG-HEAD+ s" lib/nope.f" s" 1" DWAIVER+ SB$ ;
: R-MATMUL-MEAS$ ( -- ptr u8 n )
   SB-RESET
   s" lib/ptx/cg-matmul.f" KLT-HEAD+ s" +\ tweak" SB-APPEND LF+
   REG-HEAD+ KLT-ROW+ SB$ ;

: RF-V1$ ( -- ptr u8 n )    SB-RESET s" lib/ptx/cg-mma.f" s" 1" FWAIVER+ SB$ ;
: RF-V2V1$ ( -- ptr u8 n )  SB-RESET s" lib/ptx/cg-mma.f" s" 2" FWAIVER+ s" lib/ptx/cg-mma.f" s" 1" FWAIVER+ SB$ ;
: RF-DUP$ ( -- ptr u8 n )   SB-RESET s" lib/ptx/cg-mma.f" s" 1" FWAIVER+ s" lib/ptx/cg-mma.f" s" 1" FWAIVER+ SB$ ;
: RF-EMPTY$ ( -- ptr u8 n ) SB-RESET s" # empty registry" SB-APPEND LF+ SB$ ;

: R-WRITE ( ptr u8 n -- )   \ write a registry fixture to REGFILE$
   REGFILE$ 2swap WRITE-ALL ;

: R-RUN ( ptr u8 n -- n n ) {: a:ptr u:n :}   \ diff text -- out-len rc, against REGFILE$
   KERNEL-PERF-LINT:RESET
   REGFILE$ KERNEL-PERF-LINT:REG-LOAD!
   KLT-OUT KLT-CAP LINT-OUT-BUFFER!
   a u KERNEL-PERF-LINT:SOURCE
   [: KERNEL-PERF-LINT:FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   rc ;

: RATCHET-TESTS ( -- )
   CLEANUP-RESET
   s" habu-kernel-perf-ratchet" TMPDIR-MKDIR {: ra:ptr ru:n :}
   ra ru CLEANUP-DIR+
   ra ru s" reg.tsv" REGFILE-BUF JOIN-PATH REGFILE-U !
   REGFILE$ CLEANUP+
   \ (1) unchanged historical waiver stays valid when its emitter is not touched
   RF-V1$ R-WRITE   R-MATMUL-MEAS$ R-RUN KLT-CLEAN
   \ (2) touching its emitter with no replacement fails
   RF-V1$ R-WRITE   R-NOROW$ R-RUN KLT-MISSING$ KLT-EXPECT
   \ (3) another emitter's waiver cannot satisfy the touch
   RF-V1$ R-WRITE   R-WAIVER-OTHER$ R-RUN KLT-MISSING$ KLT-EXPECT
   \ (4) a same-change newly-versioned waiver passes
   RF-V1$ R-WRITE   R-WAIVER-V2$ R-RUN KLT-CLEAN
   \ (4b) a same-change measurement passes
   RF-V1$ R-WRITE   R-MEAS$ R-RUN KLT-CLEAN
   \ reject: a waiver not newer than the standing one is stale
   RF-V2V1$ R-WRITE   R-WAIVER-V1$ R-RUN s" E-PERF-WAIVER-STALE" KLT-EXPECT
   \ reject: a duplicate live waiver in the registry
   RF-DUP$ R-WRITE   R-WAIVER-V1$ R-RUN s" E-PERF-WAIVER-DUP" KLT-EXPECT
   \ reject: an added waiver naming an unknown emitter
   RF-EMPTY$ R-WRITE   R-UNKNOWN$ R-RUN s" E-PERF-BAD-ROW" KLT-EXPECT
   CLEANUP-RUN ;

\ ---- watch-table: classify / dedup / resolve / completeness ratchet ----------

\ Each set is two blocks: the index cells and the path-byte arena.
create PW-SCRATCH-IX  PERF-WATCH:INDEX-BYTES allot
create PW-SCRATCH-AR  PERF-WATCH:ARENA-BYTES allot
create PW-MANIFEST-IX PERF-WATCH:INDEX-BYTES allot
create PW-MANIFEST-AR PERF-WATCH:ARENA-BYTES allot
variable PW-BAD

: CLASSIFY-TESTS ( -- )   \ the classifier keys every acceptance case exactly
   s" lib/ptx/tile.f"               PERF-WATCH:CLASSIFY PERF-WATCH:PW-WATCHED  T=
   s" lib/ptx/opt.f"                PERF-WATCH:CLASSIFY PERF-WATCH:PW-WATCHED  T=
   s" lib/ptx/collective.f"         PERF-WATCH:CLASSIFY PERF-WATCH:PW-WATCHED  T=
   s" lib/ptx/cg-collective.f"      PERF-WATCH:CLASSIFY PERF-WATCH:PW-WATCHED  T=
   s" src/arch/ptx/emit.f"          PERF-WATCH:CLASSIFY PERF-WATCH:PW-WATCHED  T=
   s" lib/ptx/cg-collective-test.f" PERF-WATCH:CLASSIFY PERF-WATCH:PW-TEST     T=
   s" lib/ptx/tile-test.f"          PERF-WATCH:CLASSIFY PERF-WATCH:PW-TEST     T=
   s" lib/ptx/cuda-driver.f"        PERF-WATCH:CLASSIFY PERF-WATCH:PW-EXCLUDED T=
   s" tools/ptx/saxpy-wrong-cg.f"   PERF-WATCH:CLASSIFY PERF-WATCH:PW-EXCLUDED T=
   s" lib/ptx/zz-new-producer.f"    PERF-WATCH:CLASSIFY PERF-WATCH:PW-UNKNOWN  T= ;

: DEDUP-TESTS ( -- )   \ a path set accepts distinct paths and rejects a duplicate
   PW-SCRATCH-IX PERF-WATCH:PS-RESET
   PW-SCRATCH-IX PW-SCRATCH-AR s" lib/ptx/tile.f" PERF-WATCH:PS-ADD
   PW-SCRATCH-IX PW-SCRATCH-AR s" lib/ptx/opt.f"  PERF-WATCH:PS-ADD
   PW-SCRATCH-IX PERF-WATCH:PS-N 2 T=
   [: PW-SCRATCH-IX PW-SCRATCH-AR s" lib/ptx/tile.f" PERF-WATCH:PS-ADD ;] E-WATCH-DUP TTHROWSQ ;

: PW-RESOLVE# ( ptr n ptr u8 -- n ) {: ix:ptr ar:ptr :}   \ paths in the set that do not resolve on disk
   0 PW-BAD !
   ix PERF-WATCH:PS-N 0 ?do
      ix ar i PERF-WATCH:PS-AT FILE? 0= if PW-BAD @ 1+ PW-BAD ! then
   loop PW-BAD @ ;

: RESOLVE-TESTS ( -- )   \ every watched + excluded path resolves; a bogus path is caught
   PERF-WATCH:WATCH-INDEX   PERF-WATCH:WATCH-ARENA   PW-RESOLVE# 0 T=
   PERF-WATCH:EXCLUDE-INDEX PERF-WATCH:EXCLUDE-ARENA PW-RESOLVE# 0 T=
   PW-SCRATCH-IX PERF-WATCH:PS-RESET
   PW-SCRATCH-IX PW-SCRATCH-AR s" lib/ptx/tile.f" PERF-WATCH:PS-ADD
   PW-SCRATCH-IX PW-SCRATCH-AR s" lib/ptx/zz-does-not-exist.f" PERF-WATCH:PS-ADD
   PW-SCRATCH-IX PW-SCRATCH-AR PW-RESOLVE# 1 T= ;

: PW-MAN-LIB ( ptr u8 n -- ) {: a:ptr u:n :}   \ collect every lib/src .f source
   a u s" .f" ENDS-WITH? 0= if exit then
   PW-MANIFEST-IX PW-MANIFEST-AR a u PERF-WATCH:PS-ADD ;

: PW-MAN-CG ( ptr u8 n -- ) {: a:ptr u:n :}   \ collect every tools/ptx/*-cg.f driver
   a u s" -cg.f" ENDS-WITH? 0= if exit then
   PW-MANIFEST-IX PW-MANIFEST-AR a u PERF-WATCH:PS-ADD ;

: PW-MAN-COLLECT ( -- )   \ the on-disk producer manifest across the scanned dirs
   PW-MANIFEST-IX PERF-WATCH:PS-RESET
   s" lib/ptx"      [: PW-MAN-LIB ;] WALK-FILES
   s" src/arch/ptx" [: PW-MAN-LIB ;] WALK-FILES
   s" tools/ptx"    [: PW-MAN-CG ;] WALK-FILES ;

: PW-RATCHET# ( ptr n ptr u8 -- n ) {: ix:ptr ar:ptr :}   \ unclassified producers in a manifest set
   0 PW-BAD !
   ix PERF-WATCH:PS-N 0 ?do
      ix ar i PERF-WATCH:PS-AT PERF-WATCH:CLASSIFY PERF-WATCH:PW-UNKNOWN = if PW-BAD @ 1+ PW-BAD ! then
   loop PW-BAD @ ;

: RATCHET-COMPLETE-TESTS ( -- )   \ the live producer tree is fully owned; an addition fails
   PW-MAN-COLLECT
   PW-MANIFEST-IX PW-MANIFEST-AR PW-RATCHET# 0 T=
   PW-MANIFEST-IX PW-MANIFEST-AR s" lib/ptx/zz-new-producer.f" PERF-WATCH:PS-ADD
   PW-MANIFEST-IX PW-MANIFEST-AR PW-RATCHET# 1 T= ;

T-RESET
KLT-SOURCE-TESTS
NEW-PRODUCER-TESTS
NEAR-MISS-TESTS
KLT-FILE-TESTS
MULTI-FILE-TESTS
MALFORMED-TESTS
RATCHET-TESTS
CLASSIFY-TESTS
DEDUP-TESTS
RESOLVE-TESTS
RATCHET-COMPLETE-TESTS
T-REPORT

;package
