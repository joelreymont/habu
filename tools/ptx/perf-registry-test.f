\ perf-registry-test.f - checked fixtures for the kernel profile-row registry,
\ plus validation of the committed tools/ptx/perf-rows.tsv.

require lib/test.f
require lib/string.f
require tools/ptx/perf-registry.f

package PERF-RT

9 constant PRT-TAB

: TAB+ ( -- )
   PRT-TAB SB-APPEND-C ;

: ROW-HEAD+ ( ptr u8 n -- ) {: ka:ptr ku:n :}   \ kernel + fixed test config
   ka ku SB-APPEND TAB+
   s" 4" SB-APPEND TAB+ s" 1" SB-APPEND TAB+
   s" 256" SB-APPEND TAB+ s" 1" SB-APPEND TAB+
   s" 10" SB-APPEND TAB+ s" 1024" SB-APPEND TAB+ ;

: ROW-TAIL+ ( -- )
   TAB+ s" test-dev" SB-APPEND TAB+ s" 2026-07-13" SB-APPEND TAB+
   s" fixture note" SB-APPEND ;

: GBS-ROW$ ( -- ptr u8 n )
   SB-RESET
   s" SAXPY-T" ROW-HEAD+
   s" GBS" SB-APPEND TAB+ s" 42000" SB-APPEND
   ROW-TAIL+
   SB$ ;

: WAIVER-TAIL+ ( ptr u8 n ptr u8 n -- ) {: ea:ptr eu va:ptr vu :}   \ + emitter + version
   TAB+ ea eu SB-APPEND TAB+ va vu SB-APPEND ;

: WAIVER-ROW$ ( -- ptr u8 n )
   SB-RESET
   s" ATT-T" ROW-HEAD+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND
   ROW-TAIL+
   s" lib/ptx/cg-mma.f" s" 1" WAIVER-TAIL+
   SB$ ;

: WAIVER-V2$ ( -- ptr u8 n )   \ same kernel+emitter, next version
   SB-RESET
   s" ATT-T" ROW-HEAD+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND
   ROW-TAIL+
   s" lib/ptx/cg-mma.f" s" 2" WAIVER-TAIL+
   SB$ ;

: WAIVER-BAD-NOTE$ ( -- ptr u8 n )   \ waiver without a documenting note
   SB-RESET
   s" ATT-T" ROW-HEAD+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND
   TAB+ s" test-dev" SB-APPEND TAB+ s" 2026-07-13" SB-APPEND TAB+
   s" lib/ptx/cg-mma.f" s" 1" WAIVER-TAIL+
   SB$ ;

: WAIVER-BAD-VALUE$ ( -- ptr u8 n )   \ waiver with a nonzero value
   SB-RESET
   s" ATT-T" ROW-HEAD+
   s" WAIVER" SB-APPEND TAB+ s" 7" SB-APPEND
   ROW-TAIL+
   s" lib/ptx/cg-mma.f" s" 1" WAIVER-TAIL+
   SB$ ;

: WAIVER-BAD-EMITTER$ ( -- ptr u8 n )   \ waiver naming a non-emitter path
   SB-RESET
   s" ATT-T" ROW-HEAD+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND
   ROW-TAIL+
   s" lib/foo.f" s" 1" WAIVER-TAIL+
   SB$ ;

: WAIVER-BAD-WVID$ ( -- ptr u8 n )   \ waiver with version below 1
   SB-RESET
   s" ATT-T" ROW-HEAD+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND
   ROW-TAIL+
   s" lib/ptx/cg-mma.f" s" 0" WAIVER-TAIL+
   SB$ ;

: WAIVER-SHORT$ ( -- ptr u8 n )   \ waiver missing the emitter/version identity
   SB-RESET
   s" ATT-T" ROW-HEAD+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND
   ROW-TAIL+
   SB$ ;

: WAIVER-REORDER$ ( -- ptr u8 n )   \ version and emitter transposed (forged identity)
   SB-RESET
   s" ATT-T" ROW-HEAD+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND
   ROW-TAIL+
   s" 1" s" lib/ptx/cg-mma.f" WAIVER-TAIL+
   SB$ ;

: BAD-METRIC$ ( -- ptr u8 n )
   SB-RESET
   s" SAXPY-T" ROW-HEAD+
   s" XXX" SB-APPEND TAB+ s" 42000" SB-APPEND
   ROW-TAIL+
   SB$ ;

: BAD-VALUE$ ( -- ptr u8 n )   \ zero value on a metric row
   SB-RESET
   s" SAXPY-T" ROW-HEAD+
   s" GBS" SB-APPEND TAB+ s" 0" SB-APPEND
   ROW-TAIL+
   SB$ ;

: BAD-DATE$ ( -- ptr u8 n )
   SB-RESET
   s" SAXPY-T" ROW-HEAD+
   s" GBS" SB-APPEND TAB+ s" 42000" SB-APPEND
   TAB+ s" test-dev" SB-APPEND TAB+ s" 13-07-2026" SB-APPEND TAB+
   s" fixture note" SB-APPEND
   SB$ ;

: BAD-DATE-RANGE$ ( -- ptr u8 n )   \ well-formed digits/dashes but month/day out of range
   SB-RESET
   s" SAXPY-T" ROW-HEAD+
   s" GBS" SB-APPEND TAB+ s" 42000" SB-APPEND
   TAB+ s" test-dev" SB-APPEND TAB+ s" 2026-99-99" SB-APPEND TAB+
   s" fixture note" SB-APPEND
   SB$ ;

: SHORT-ROW$ ( -- ptr u8 n )   \ missing fields
   SB-RESET
   s" SAXPY-T" SB-APPEND TAB+ s" 4" SB-APPEND TAB+ s" GBS" SB-APPEND
   SB$ ;

: LONG-ROW$ ( -- ptr u8 n )   \ 13th field
   SB-RESET
   s" SAXPY-T" ROW-HEAD+
   s" GBS" SB-APPEND TAB+ s" 42000" SB-APPEND
   ROW-TAIL+
   TAB+ s" extra" SB-APPEND
   SB$ ;

: PRT-BAD-METRIC ( -- )
   BAD-METRIC$ PERF:ADD-LINE ;

: PRT-BAD-VALUE ( -- )
   BAD-VALUE$ PERF:ADD-LINE ;

: PRT-BAD-DATE ( -- )
   BAD-DATE$ PERF:ADD-LINE ;

: PRT-BAD-DATE-RANGE ( -- )
   BAD-DATE-RANGE$ PERF:ADD-LINE ;

: PRT-OOB-READ ( -- )   \ reading a field at row index == ROW# is stale, must fail closed
   PERF:ROW# PERF:GRID@ drop ;

: PRT-SHORT ( -- )
   SHORT-ROW$ PERF:ADD-LINE ;

: PRT-LONG ( -- )
   LONG-ROW$ PERF:ADD-LINE ;

: PRT-WAIVER-NOTE ( -- )
   WAIVER-BAD-NOTE$ PERF:ADD-LINE ;

: PRT-WAIVER-VALUE ( -- )
   WAIVER-BAD-VALUE$ PERF:ADD-LINE ;

: PRT-WAIVER-EMITTER ( -- )
   WAIVER-BAD-EMITTER$ PERF:ADD-LINE ;

: PRT-WAIVER-WVID ( -- )
   WAIVER-BAD-WVID$ PERF:ADD-LINE ;

: PRT-WAIVER-SHORT ( -- )
   WAIVER-SHORT$ PERF:ADD-LINE ;

: PRT-WAIVER-REORDER ( -- )
   WAIVER-REORDER$ PERF:ADD-LINE ;

: FIND-ROW ( ptr u8 n n -- n ) {: ka:ptr ku:n m:n :}   \ first row idx by kernel+metric, -1 if absent
   0 begin dup PERF:ROW# < while
      dup PERF:KERNEL$ ka ku STR=
      if dup PERF:METRIC@ m = if exit then then
      1+
   repeat drop -1 ;

: PRT-PARSE-TESTS ( -- )
   PERF:RESET
   GBS-ROW$ PERF:ADD-LINE
   PERF:ROW# 1 T=
   0 PERF:KERNEL$ s" SAXPY-T" T$=
   0 PERF:GRID@ 4 T=
   0 PERF:GRIDY@ 1 T=
   0 PERF:BLOCK@ 256 T=
   0 PERF:BLOCKY@ 1 T=
   0 PERF:ITERS@ 10 T=
   0 PERF:WORK@ 1024 T=
   0 PERF:METRIC@ PERF:M-GBS T=
   0 PERF:VALUE@ 42000 T=
   0 PERF:DEVICE$ s" test-dev" T$=
   0 PERF:DATE$ s" 2026-07-13" T$=
   0 PERF:NOTE$ s" fixture note" T$=
   0 PERF:WAIVER? TFALSE
   s" # a comment" PERF:ADD-LINE
   s"   " PERF:ADD-LINE
   PERF:ROW# 1 T=
   WAIVER-ROW$ PERF:ADD-LINE
   PERF:ROW# 2 T=
   1 PERF:WAIVER? TTRUE
   1 PERF:VALUE@ 0 T=
   1 PERF:EMITTER$ s" lib/ptx/cg-mma.f" T$=
   1 PERF:WVID@ 1 T= ;

: PRT-KEY-TESTS ( -- )
   PERF:RESET
   GBS-ROW$ PERF:ADD-LINE
   GBS-ROW$ PERF:ADD-LINE
   0 1 PERF:KEY= TTRUE
   WAIVER-ROW$ PERF:ADD-LINE
   0 2 PERF:KEY= TFALSE ;

: PRT-LINE-TESTS ( -- )
   s" # comment" PERF:LINE-DATA? TFALSE
   s" " PERF:LINE-DATA? TFALSE
   GBS-ROW$ PERF:LINE-DATA? TTRUE
   GBS-ROW$ PERF:LINE-OK? TTRUE
   s" # comment" PERF:LINE-OK? TTRUE
   BAD-METRIC$ PERF:LINE-OK? TFALSE
   SHORT-ROW$ PERF:LINE-OK? TFALSE ;

: PRT-REJECT-TESTS ( -- )
   PERF:RESET
   [: PRT-BAD-METRIC ;] E-PERF-ROW TTHROWSQ
   [: PRT-BAD-VALUE ;] E-PERF-ROW TTHROWSQ
   [: PRT-BAD-DATE ;] E-PERF-ROW TTHROWSQ
   [: PRT-BAD-DATE-RANGE ;] E-PERF-ROW TTHROWSQ
   [: PRT-SHORT ;] E-PERF-ROW TTHROWSQ
   [: PRT-LONG ;] E-PERF-ROW TTHROWSQ
   [: PRT-WAIVER-NOTE ;] E-PERF-ROW TTHROWSQ
   [: PRT-WAIVER-VALUE ;] E-PERF-ROW TTHROWSQ
   [: PRT-WAIVER-EMITTER ;] E-PERF-ROW TTHROWSQ
   [: PRT-WAIVER-WVID ;] E-PERF-ROW TTHROWSQ
   [: PRT-WAIVER-SHORT ;] E-PERF-ROW TTHROWSQ
   [: PRT-WAIVER-REORDER ;] E-PERF-ROW TTHROWSQ
   PERF:ROW# 0 T= ;

: PRT-WAIVER-DUP-TESTS ( -- )   \ duplicate live waiver detection over the row set
   PERF:RESET
   WAIVER-ROW$ PERF:ADD-LINE
   PERF:WAIVER-DUP? TFALSE
   WAIVER-V2$ PERF:ADD-LINE            \ same kernel+emitter, distinct version: not a dup
   PERF:WAIVER-DUP? TFALSE
   WAIVER-ROW$ PERF:ADD-LINE           \ re-added identical identity: duplicate live waiver
   PERF:WAIVER-DUP? TTRUE ;

: PRT-BOUNDS-TESTS ( -- )   \ ROW@ rejects reads past the committed frontier
   PERF:RESET
   GBS-ROW$ PERF:ADD-LINE
   0 PERF:GRID@ 4 T=                          \ committed row 0 reads fine
   [: PRT-OOB-READ ;] E-PERF-CAP TTHROWSQ ;   \ row index == ROW# fails closed

: PRT-COMMITTED-TESTS ( -- )   \ the committed registry parses and holds the seeded rows
   s" tools/ptx/perf-rows.tsv" PERF:LOAD
   PERF:ROW# 24 > TTRUE
   s" SAXPY-V4" PERF:M-GBS FIND-ROW {: sx:n :}
   sx 0 < TFALSE
   sx PERF:VALUE@ 64209 T=
   sx PERF:DEVICE$ s" orin-nx-15w" T$=
   s" MM-CPASYNC" PERF:M-GFLOPS FIND-ROW {: mm:n :}
   mm 0 < TFALSE
   s" ATTENTION" PERF:M-GFLOPS FIND-ROW {: at:n :}   \ waiver retired: measured row present
   at 0 < TFALSE
   at PERF:VALUE@ 20529 T=
   at PERF:DEVICE$ s" orin-nx-25w" T$=
   s" ATTENTION" PERF:M-WAIVER FIND-ROW 0 < TTRUE    \ and no ATTENTION WAIVER row remains
   PERF:WAIVER-DUP? TFALSE             \ the committed registry carries no duplicate waiver
   PERF:LINE@ 0 > TTRUE                \ diagnostic accessors report the last parsed line
   PERF:LAST-LINE$ nip 0 > TTRUE ;

T-RESET
PRT-PARSE-TESTS
PRT-KEY-TESTS
PRT-LINE-TESTS
PRT-REJECT-TESTS
PRT-WAIVER-DUP-TESTS
PRT-BOUNDS-TESTS
PRT-COMMITTED-TESTS
T-REPORT

;package
