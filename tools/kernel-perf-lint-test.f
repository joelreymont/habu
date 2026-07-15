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

: KLT-WAIVER+ ( -- )   \ one valid added WAIVER registry row
   s" +KLT-KERNEL" SB-APPEND TAB+
   s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" test-dev" SB-APPEND TAB+ s" 2026-07-13" SB-APPEND TAB+
   s" device-gated: fixture waiver" SB-APPEND LF+ ;

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

: UNRELATED$ ( -- ptr u8 n )
   SB-RESET
   s" lib/ptx/tile.f" KLT-HEAD+
   s" +\ tile DSL change, not codegen" SB-APPEND LF+
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
   KLT-FILE$ s" lib/ptx/cg-a.f" MODE$ WRITE-ALL
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
   [: MALFORMED ;] DIFF:E-SYNTAX TTHROWSQ
   [: SPOOF ;] DIFF:E-SYNTAX TTHROWSQ
   [: CONTROL ;] DIFF:E-SYNTAX TTHROWSQ ;

T-RESET
KLT-SOURCE-TESTS
KLT-FILE-TESTS
MULTI-FILE-TESTS
MALFORMED-TESTS
T-REPORT

;package
