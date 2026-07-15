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
require tools/lint/diff-frame-write.f
require tools/ptx/perf-registry.f
require tools/kernel-perf-lint-core.f

package KPL-TEST
private

4096 constant CAP
9 constant TAB
16777217 constant BIG-ID-U

create OUT CAP allot
create FILE-BUF FS-PATH-CAP allot
create FRAME CAP 4 * allot

variable FILE-U
PTR-VARIABLE SRC-A
variable SRC-U
PTR-VARIABLE BIG-FRAME-A
variable BIG-FILL-U
variable LE-I
variable DIGEST-I

: SRC-A@ ( -- ptr u8 )
   SRC-A @ ;

: SRC-A! ( ptr u8 -- )
   SRC-A ! ;

: FRAME-START ( -- )
   FRAME CAP 4 *
   s" 0123456789012345678901234567890123456789"
   s" abcdef0123abcdef0123abcdef0123abcdef0123" DIFF-WRITE:START ;

: FRAME-END ( -- ptr u8 n )
   DIFF-WRITE:FINISH ;

: MODIFIED+ ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n raw:ptr rawu:n :}
   DIFF-STATUS:MODIFIED DIFF-FORM:TEXT true false
   true path pathu true path pathu raw rawu DIFF-WRITE:SECTION ;

: REMOVED+ ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n raw:ptr rawu:n :}
   DIFF-STATUS:REMOVED DIFF-FORM:TEXT true false
   true path pathu false s" " raw rawu DIFF-WRITE:SECTION ;

: RENAMED-TEXT+ ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: old:ptr oldu:n new:ptr newu:n raw:ptr rawu:n :}
   DIFF-STATUS:RENAMED DIFF-FORM:TEXT true false
   true old oldu true new newu raw rawu DIFF-WRITE:SECTION ;

: PURE-RENAME+ ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: old:ptr oldu:n new:ptr newu:n raw:ptr rawu:n :}
   DIFF-STATUS:RENAMED DIFF-FORM:PURE false false
   true old oldu true new newu raw rawu DIFF-WRITE:SECTION ;

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
   FRAME-START
   SB-RESET
   s" lib/ptx/cg-matmul.f" HEAD+
   s" +\ tweak the tile inner loop" SB-APPEND LF+
   s" lib/ptx/cg-matmul.f" SB$ MODIFIED+
   FRAME-END ;

: WATCHED+ROW$ ( -- ptr u8 n )
   FRAME-START
   SB-RESET
   s" lib/ptx/cg-matmul.f" HEAD+
   s" +\ tweak the tile inner loop" SB-APPEND LF+
   s" lib/ptx/cg-matmul.f" SB$ MODIFIED+
   SB-RESET
   s" tools/ptx/perf-rows.tsv" HEAD+
   ROW+
   s" tools/ptx/perf-rows.tsv" SB$ MODIFIED+
   FRAME-END ;

: WATCHED+WAIVER$ ( -- ptr u8 n )
   FRAME-START
   SB-RESET
   s" lib/ptx/cg-matmul.f" HEAD+
   s" +\ tweak the tile inner loop" SB-APPEND LF+
   s" lib/ptx/cg-matmul.f" SB$ MODIFIED+
   SB-RESET
   s" tools/ptx/perf-rows.tsv" HEAD+
   WAIVER+
   s" tools/ptx/perf-rows.tsv" SB$ MODIFIED+
   FRAME-END ;

: WATCHED+COMMENT$ ( -- ptr u8 n )   \ registry touched but only a comment added
   FRAME-START
   SB-RESET
   s" lib/ptx/cg-matmul.f" HEAD+
   s" +\ tweak the tile inner loop" SB-APPEND LF+
   s" lib/ptx/cg-matmul.f" SB$ MODIFIED+
   SB-RESET
   s" tools/ptx/perf-rows.tsv" HEAD+
   s" +# just a comment" SB-APPEND LF+
   s" tools/ptx/perf-rows.tsv" SB$ MODIFIED+
   FRAME-END ;

: UNRELATED$ ( -- ptr u8 n )
   FRAME-START
   SB-RESET
   s" lib/ptx/tile.f" HEAD+
   s" +\ tile DSL change, not codegen" SB-APPEND LF+
   s" lib/ptx/tile.f" SB$ MODIFIED+
   FRAME-END ;

: TOOLSCG$ ( -- ptr u8 n )
   FRAME-START
   SB-RESET
   s" tools/ptx/saxpy-v4-cg.f" HEAD+
   s" +\ unroll one more chunk" SB-APPEND LF+
   s" tools/ptx/saxpy-v4-cg.f" SB$ MODIFIED+
   FRAME-END ;

: EMIT$ ( -- ptr u8 n )
   FRAME-START
   SB-RESET
   s" src/arch/ptx/emit.f" HEAD+
   s" +\ encoder tweak" SB-APPEND LF+
   s" src/arch/ptx/emit.f" SB$ MODIFIED+
   FRAME-END ;

: DELETED$ ( -- ptr u8 n )   \ deleting a kernel emitter is a perf-relevant change
   FRAME-START
   SB-RESET
   s" diff --git a/lib/ptx/cg-vec.f b/lib/ptx/cg-vec.f" SB-APPEND LF+
   s" deleted file mode 100644" SB-APPEND LF+
   s" index abcdef1234..0000000000" SB-APPEND LF+
   s" --- a/lib/ptx/cg-vec.f" SB-APPEND LF+
   s" +++ /dev/null" SB-APPEND LF+
   s" @@ -1 +0,0 @@" SB-APPEND LF+
   s" -\ gone" SB-APPEND LF+
   s" lib/ptx/cg-vec.f" SB$ REMOVED+
   FRAME-END ;

: RENAMED-AWAY$ ( -- ptr u8 n )
   FRAME-START
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
   s" lib/ptx/cg-vec.f" s" lib/ptx/tile.f" SB$ RENAMED-TEXT+
   FRAME-END ;

: META-RENAMED$ ( -- ptr u8 n )
   FRAME-START
   SB-RESET
   s" diff --git a/lib/ptx/cg-vec.f b/lib/ptx/tile.f" SB-APPEND LF+
   s" similarity index 100%" SB-APPEND LF+
   s" rename from lib/ptx/cg-vec.f" SB-APPEND LF+
   s" rename to lib/ptx/tile.f" SB-APPEND LF+
   s" lib/ptx/cg-vec.f" s" lib/ptx/tile.f" SB$ PURE-RENAME+
   SB-RESET
   s" lib/unrelated.f" HEAD+
   s" +\ unrelated source change" SB-APPEND LF+
   s" lib/unrelated.f" SB$ MODIFIED+
   FRAME-END ;

: BADROW$ ( -- ptr u8 n )
   FRAME-START
   SB-RESET
   s" tools/ptx/perf-rows.tsv" HEAD+
   s" +KERNEL not-a-valid-row" SB-APPEND LF+
   s" tools/ptx/perf-rows.tsv" SB$ MODIFIED+
   FRAME-END ;

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
   WATCHED+ROW$ RUN CLEAN
   WATCHED+WAIVER$ RUN CLEAN
   UNRELATED$ RUN CLEAN ;

: FILE$ ( -- ptr u8 n )
   FILE-BUF FILE-U @ ;

: FILL-A ( ptr u8 n -- ) {: a:ptr u:n :}
   $61 a c!
   1 BIG-FILL-U !
   begin BIG-FILL-U @ u < while
      u BIG-FILL-U @ - BIG-FILL-U @ < if
         u BIG-FILL-U @ -
      else
         BIG-FILL-U @
      then {: chunk:n :}
      a a BIG-FILL-U @ + chunk BYTE-COPY
      chunk BIG-FILL-U +!
   repeat ;

: LE64! ( n ptr u8 -- ) {: value:n dst:ptr :}
   0 LE-I !
   begin LE-I @ 8 < while
      value LE-I @ 8 * rshift $FF and dst LE-I @ + c!
      LE-I @ 1+ LE-I !
   repeat ;

: HEX-NIBBLE ( n -- n ) {: c:n :}
   c $30 >= c $39 <= and if c $30 - exit then
   c $61 >= c $66 <= and if c $61 - 10 + exit then
   E-DIFF-SYNTAX throw ;

: DIGEST! ( ptr u8 -- ) {: dst:ptr :}
   s" 83565123bd1ca7d1c944e3ab713d599b85455c21a0321df82f0bf5a84ba4a75f"
   {: hex:ptr hexu:n :}
   hexu 64 <> if E-DIFF-SYNTAX throw then
   0 DIGEST-I !
   begin DIGEST-I @ 32 < while
      hex DIGEST-I @ 2 * + c@ HEX-NIBBLE 4 lshift
      hex DIGEST-I @ 2 * 1+ + c@ HEX-NIBBLE or
      dst DIGEST-I @ + c!
      DIGEST-I @ 1+ DIGEST-I !
   repeat ;

: BIG-ARTIFACT ( -- ptr u8 n )
   BIG-ID-U 10 DIFF-WRITE:HEADER-SIZE DIFF-WRITE:FINISH-SIZE {: cap:n :}
   cap MEM-ALLOC-BYTES drop BIG-FRAME-A !
   s" HABUDIF2" drop BIG-FRAME-A @ 8 BYTE-COPY
   1 BIG-FRAME-A @ 8 + c!
   9 begin dup 16 < while BIG-FRAME-A @ over + 0 swap c! 1+ repeat drop
   BIG-ID-U BIG-FRAME-A @ 16 + LE64!
   BIG-FRAME-A @ 24 + BIG-ID-U FILL-A
   10 BIG-FRAME-A @ BIG-ID-U 24 + + LE64!
   s" abcdef0123" drop BIG-FRAME-A @ BIG-ID-U 32 + + 10 BYTE-COPY
   $54 BIG-FRAME-A @ BIG-ID-U 42 + + c!
   0 BIG-FRAME-A @ BIG-ID-U 43 + + LE64!
   BIG-FRAME-A @ BIG-ID-U 51 + + DIGEST!
   cap 16777216 > TTRUE
   BIG-FRAME-A @ 8 s" HABUDIF2" T$=
   BIG-FRAME-A @ cap ;

: BIG-FILE-TEST ( ptr u8 n -- ) {: root:ptr rootu:n :}
   root rootu s" big.hbdiff" FILE-BUF JOIN-PATH FILE-U !
   FILE$ CLEANUP+
   FILE$ BIG-ARTIFACT WRITE-ALL
   FILE$ DIFF-FILE:LOAD {: a:ptr u:n :}
   u 16777216 > TTRUE
   a 8 s" HABUDIF2" T$= ;

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
   ra ru BIG-FILE-TEST
   CLEANUP-RUN ;

T-RESET
SOURCE-TESTS
FILE-TESTS
T-REPORT

;package
