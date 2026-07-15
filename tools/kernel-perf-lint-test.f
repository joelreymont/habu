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
require tools/lint/diff.f
require tools/ptx/perf-registry.f
require tools/kernel-perf-lint-core.f

package KPL-TEST
private

4096 constant CAP
9 constant TAB

create OUT CAP allot
create FILE-BUF FS-PATH-CAP allot

variable FILE-U
variable SRC-A
variable SRC-U

: SRC-A-FIELD ( -- ptr ptr u8 )
   SRC-A 0 ptr-field ;

: SRC-A@ ( -- ptr u8 )
   SRC-A-FIELD @ ;

: SRC-A! ( ptr u8 -- )
   SRC-A-FIELD ! ;

: TAB+ ( -- )
   TAB SB-APPEND-C ;

: LF+ ( -- )
   10 SB-APPEND-C ;

: HEAD+ ( ptr u8 n -- ) {: pa:ptr pu:n :}
   s" diff --git a/" SB-APPEND pa pu SB-APPEND
   s"  b/" SB-APPEND pa pu SB-APPEND LF+
   s" index 1234567890..abcdef1234 100644" SB-APPEND LF+
   s" --- a/" SB-APPEND pa pu SB-APPEND LF+
   s" +++ b/" SB-APPEND pa pu SB-APPEND LF+
   s" @@ -0,0 +1 @@" SB-APPEND LF+ ;

: ROW+ ( -- )   \ one valid added GBS registry row
   s" +KERNEL" SB-APPEND TAB+
   s" 4" SB-APPEND TAB+ s" 1" SB-APPEND TAB+
   s" 256" SB-APPEND TAB+ s" 1" SB-APPEND TAB+
   s" 10" SB-APPEND TAB+ s" 1024" SB-APPEND TAB+
   s" GBS" SB-APPEND TAB+ s" 42000" SB-APPEND TAB+
   s" test-dev" SB-APPEND TAB+ s" 2026-07-13" SB-APPEND TAB+
   s" lint fixture" SB-APPEND LF+ ;

: WAIVER+ ( -- )   \ one valid added WAIVER registry row
   s" +KERNEL" SB-APPEND TAB+
   s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" test-dev" SB-APPEND TAB+ s" 2026-07-13" SB-APPEND TAB+
   s" device-gated: fixture waiver" SB-APPEND LF+ ;

: WATCHED-NOROW$ ( -- ptr u8 n )
   SB-RESET
   s" lib/ptx/cg-matmul.f" HEAD+
   s" +\ tweak the tile inner loop" SB-APPEND LF+
   SB$ ;

: WATCHED+ROW$ ( -- ptr u8 n )
   SB-RESET
   s" lib/ptx/cg-matmul.f" HEAD+
   s" +\ tweak the tile inner loop" SB-APPEND LF+
   s" tools/ptx/perf-rows.tsv" HEAD+
   ROW+
   SB$ ;

: WATCHED+WAIVER$ ( -- ptr u8 n )
   SB-RESET
   s" lib/ptx/cg-matmul.f" HEAD+
   s" +\ tweak the tile inner loop" SB-APPEND LF+
   s" tools/ptx/perf-rows.tsv" HEAD+
   WAIVER+
   SB$ ;

: WATCHED+COMMENT$ ( -- ptr u8 n )   \ registry touched but only a comment added
   SB-RESET
   s" lib/ptx/cg-matmul.f" HEAD+
   s" +\ tweak the tile inner loop" SB-APPEND LF+
   s" tools/ptx/perf-rows.tsv" HEAD+
   s" +# just a comment" SB-APPEND LF+
   SB$ ;

: UNRELATED$ ( -- ptr u8 n )
   SB-RESET
   s" lib/ptx/tile.f" HEAD+
   s" +\ tile DSL change, not codegen" SB-APPEND LF+
   SB$ ;

: TOOLSCG$ ( -- ptr u8 n )
   SB-RESET
   s" tools/ptx/saxpy-v4-cg.f" HEAD+
   s" +\ unroll one more chunk" SB-APPEND LF+
   SB$ ;

: EMIT$ ( -- ptr u8 n )
   SB-RESET
   s" src/arch/ptx/emit.f" HEAD+
   s" +\ encoder tweak" SB-APPEND LF+
   SB$ ;

: DELETED$ ( -- ptr u8 n )   \ deleting a kernel emitter is a perf-relevant change
   SB-RESET
   s" diff --git a/lib/ptx/cg-vec.f b/lib/ptx/cg-vec.f" SB-APPEND LF+
   s" deleted file mode 100644" SB-APPEND LF+
   s" index abcdef1234..0000000000" SB-APPEND LF+
   s" --- a/lib/ptx/cg-vec.f" SB-APPEND LF+
   s" +++ /dev/null" SB-APPEND LF+
   s" @@ -1 +0,0 @@" SB-APPEND LF+
   s" -\ gone" SB-APPEND LF+
   SB$ ;

: RENAMED-AWAY$ ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/lib/ptx/cg-vec.f b/lib/ptx/tile.f" SB-APPEND LF+
   s" similarity index 50%" SB-APPEND LF+
   s" rename from lib/ptx/cg-vec.f" SB-APPEND LF+
   s" rename to lib/ptx/tile.f" SB-APPEND LF+
   s" index 1234567890..abcdef1234 100644" SB-APPEND LF+
   s" --- a/lib/ptx/cg-vec.f" SB-APPEND LF+
   s" +++ b/lib/ptx/tile.f" SB-APPEND LF+
   s" @@ -1 +1 @@" SB-APPEND LF+
   s" -\ old emitter" SB-APPEND LF+
   s" +\ unrelated destination" SB-APPEND LF+
   SB$ ;

: META-RENAMED$ ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/lib/ptx/cg-vec.f b/lib/ptx/tile.f" SB-APPEND LF+
   s" similarity index 100%" SB-APPEND LF+
   s" rename from lib/ptx/cg-vec.f" SB-APPEND LF+
   s" rename to lib/ptx/tile.f" SB-APPEND LF+
   s" lib/unrelated.f" HEAD+
   s" +\ unrelated source change" SB-APPEND LF+
   SB$ ;

: BADROW$ ( -- ptr u8 n )
   SB-RESET
   s" tools/ptx/perf-rows.tsv" HEAD+
   s" +KERNEL not-a-valid-row" SB-APPEND LF+
   SB$ ;

: SPOOF$ ( -- ptr u8 n )
   SB-RESET
   s" lib/ptx/cg-matmul.f" HEAD+
   s" +\ tweak the tile inner loop" SB-APPEND LF+
   s" +++ b/tools/ptx/perf-rows.tsv" SB-APPEND LF+
   ROW+
   SB$ ;

: SRC! ( ptr u8 n -- ) {: a:ptr u:n :}
   a SRC-A! u SRC-U ! ;

: RUN-SOURCE ( -- )
   SRC-A@ SRC-U @ KERNEL-PERF-LINT:SOURCE
   KERNEL-PERF-LINT:FINISH ;

: RUN ( ptr u8 n -- n n ) {: a:ptr u:n :}   \ diff text -- out-len rc
   KERNEL-PERF-LINT:RESET
   OUT CAP LINT-OUT-BUFFER!
   a u SRC!
   [: RUN-SOURCE ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   rc ;

: CLEAN ( n n -- ) {: outu:n rc:n :}
   rc 0 T=
   outu 0 T= ;

: EXPECT ( n n ptr u8 n -- ) {: outu:n rc:n ma:ptr mu:n :}
   rc 1 T=
   OUT outu ma mu CONTAINS? TTRUE ;

: SYNTAX ( n n -- ) {: outu:n rc:n :}
   rc E-DIFF-SYNTAX T=
   outu 0 T= ;

: MISSING$ ( -- ptr u8 n )
   s" E-PERF-ROW-MISSING" ;

: SOURCE-TESTS ( -- )
   WATCHED-NOROW$ RUN MISSING$ EXPECT
   TOOLSCG$ RUN MISSING$ EXPECT
   EMIT$ RUN MISSING$ EXPECT
   DELETED$ RUN MISSING$ EXPECT
   RENAMED-AWAY$ RUN MISSING$ EXPECT
   META-RENAMED$ RUN MISSING$ EXPECT
   WATCHED+COMMENT$ RUN MISSING$ EXPECT
   BADROW$ RUN s" E-PERF-BAD-ROW" EXPECT
   SPOOF$ RUN SYNTAX
   WATCHED+ROW$ RUN CLEAN
   WATCHED+WAIVER$ RUN CLEAN
   UNRELATED$ RUN CLEAN ;

: FILE$ ( -- ptr u8 n )
   FILE-BUF FILE-U @ ;

: FILE-TESTS ( -- )   \ the file entrypoint reports the same finding
   CLEANUP-RESET
   s" habu-kernel-perf-lint" TMPDIR-MKDIR {: ra:ptr ru:n :}
   ra ru CLEANUP-DIR+
   ra ru s" watched.diff" FILE-BUF JOIN-PATH FILE-U !
   FILE$ CLEANUP+
   FILE$ WATCHED-NOROW$ WRITE-ALL
   KERNEL-PERF-LINT:RESET
   OUT CAP LINT-OUT-BUFFER!
   FILE$ KERNEL-PERF-LINT:FILE
   [: KERNEL-PERF-LINT:FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   rc MISSING$ EXPECT
   CLEANUP-RUN ;

T-RESET
SOURCE-TESTS
FILE-TESTS
T-REPORT

;package
