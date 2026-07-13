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

: WAIVER-ROW$ ( -- ptr u8 n )
   SB-RESET
   s" ATT-T" ROW-HEAD+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND
   ROW-TAIL+
   SB$ ;

: WAIVER-BAD-NOTE$ ( -- ptr u8 n )   \ waiver without a documenting note
   SB-RESET
   s" ATT-T" ROW-HEAD+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND
   TAB+ s" test-dev" SB-APPEND TAB+ s" 2026-07-13" SB-APPEND TAB+
   SB$ ;

: WAIVER-BAD-VALUE$ ( -- ptr u8 n )   \ waiver with a nonzero value
   SB-RESET
   s" ATT-T" ROW-HEAD+
   s" WAIVER" SB-APPEND TAB+ s" 7" SB-APPEND
   ROW-TAIL+
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
   1 PERF:VALUE@ 0 T= ;

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
   PERF:ROW# 0 T= ;

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
   s" ATTENTION" PERF:M-WAIVER FIND-ROW {: at:n :}
   at 0 < TFALSE
   at PERF:WAIVER? TTRUE
   at PERF:NOTE$ nip 0= TFALSE
   PERF:LINE@ 0 > TTRUE                \ diagnostic accessors report the last parsed line
   PERF:LAST-LINE$ nip 0 > TTRUE ;

T-RESET
PRT-PARSE-TESTS
PRT-KEY-TESTS
PRT-LINE-TESTS
PRT-REJECT-TESTS
PRT-BOUNDS-TESTS
PRT-COMMITTED-TESTS
T-REPORT

;package
