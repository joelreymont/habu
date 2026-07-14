\ perf-compare-test.f - checked fixtures for the perf-regression compare tool:
\ improve, regress, tolerance-edge, and missing-row cases.

require lib/test.f
require lib/string.f
require lib/fmt.f
require tools/ptx/perf-compare.f

package PERF-CT

9 constant PCT-TAB

: TAB+ ( -- )
   PCT-TAB SB-APPEND-C ;

: CT-ROW+ ( ptr u8 n n -- ) {: ka:ptr ku:n v:n :}   \ add a GFLOPS row: kernel ka/ku, value v
   SB-RESET
   ka ku SB-APPEND TAB+
   s" 8" SB-APPEND TAB+ s" 8" SB-APPEND TAB+
   s" 16" SB-APPEND TAB+ s" 16" SB-APPEND TAB+
   s" 10" SB-APPEND TAB+ s" 4096" SB-APPEND TAB+
   s" GFLOPS" SB-APPEND TAB+ v SB-INT TAB+
   s" test-dev" SB-APPEND TAB+ s" 2026-07-13" SB-APPEND TAB+
   s" compare fixture" SB-APPEND
   SB$ PERF:ADD-LINE ;

: CT-ROW-DEV+ ( ptr u8 n ptr u8 n n -- ) {: ka:ptr ku:n da:ptr du:n v:n :}   \ GFLOPS row kernel ka/ku on device da/du, value v
   SB-RESET
   ka ku SB-APPEND TAB+
   s" 8" SB-APPEND TAB+ s" 8" SB-APPEND TAB+
   s" 16" SB-APPEND TAB+ s" 16" SB-APPEND TAB+
   s" 10" SB-APPEND TAB+ s" 4096" SB-APPEND TAB+
   s" GFLOPS" SB-APPEND TAB+ v SB-INT TAB+
   da du SB-APPEND TAB+ s" 2026-07-13" SB-APPEND TAB+
   s" device isolation fixture" SB-APPEND
   SB$ PERF:ADD-LINE ;

: CT-WAIVER+ ( ptr u8 n -- ) {: ka:ptr ku:n :}
   SB-RESET
   ka ku SB-APPEND TAB+
   s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" 0" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" WAIVER" SB-APPEND TAB+ s" 0" SB-APPEND TAB+
   s" test-dev" SB-APPEND TAB+ s" 2026-07-13" SB-APPEND TAB+
   s" device-gated waiver fixture" SB-APPEND
   SB$ PERF:ADD-LINE ;

: CT-VERDICT-TESTS ( -- )   \ value-level verdicts around the 5% band
   1000 1000 PERF:VERDICT PERF:V-OK T=
   1000 950 PERF:VERDICT PERF:V-OK T=        \ tolerance edge: exactly -5% stays OK
   1000 949 PERF:VERDICT PERF:V-REGRESS T=
   1000 1050 PERF:VERDICT PERF:V-OK T=       \ tolerance edge: exactly +5% stays OK
   1000 1051 PERF:VERDICT PERF:V-IMPROVE T=
   64209 64209 PERF:VERDICT PERF:V-OK T= ;

: CT-BAD-VERDICT ( -- )
   0 1000 PERF:VERDICT drop ;

: CT-MISSING-TESTS ( -- )
   PERF:RESET
   s" KA" 1000 CT-ROW+
   0 PERF:BASELINE -1 T=
   0 PERF:ROW-VERDICT PERF:V-MISSING T= ;

: CT-IMPROVE-TESTS ( -- )
   PERF:RESET
   s" KA" 1000 CT-ROW+
   s" KA" 1100 CT-ROW+
   1 PERF:BASELINE 0 T=
   1 PERF:ROW-VERDICT PERF:V-IMPROVE T= ;

: CT-REGRESS-TESTS ( -- )
   PERF:RESET
   s" KA" 1000 CT-ROW+
   s" KA" 1100 CT-ROW+
   s" KA" 900 CT-ROW+                        \ 900 vs latest baseline 1100 -> regress
   2 PERF:BASELINE 1 T=
   2 PERF:ROW-VERDICT PERF:V-REGRESS T=
   PERF:SCAN 1 T=                            \ only the latest pair per key is compared
   s" KB" 500 CT-ROW+                        \ unrelated single-row key
   3 PERF:ROW-VERDICT PERF:V-MISSING T=
   PERF:SCAN 1 T= ;

: CT-EDGE-TESTS ( -- )
   PERF:RESET
   s" KA" 1000 CT-ROW+
   s" KA" 950 CT-ROW+
   1 PERF:ROW-VERDICT PERF:V-OK T=
   PERF:SCAN 0 T= ;

: CT-KEY-MISMATCH ( -- )
   0 1 PERF:COMPARE-ROWS drop ;

: CT-KEY-TESTS ( -- )
   PERF:RESET
   s" KA" 1000 CT-ROW+
   s" KB" 1000 CT-ROW+
   [: CT-KEY-MISMATCH ;] E-PERF-KEY TTHROWSQ
   [: CT-BAD-VERDICT ;] E-PERF-ROW TTHROWSQ ;

: CT-WAIVER-TESTS ( -- )
   PERF:RESET
   s" KW" CT-WAIVER+
   s" KW" CT-WAIVER+
   1 PERF:ROW-VERDICT PERF:V-OK T=           \ waivers never regress
   PERF:SCAN 0 T= ;

: CT-DEVICE-ISO-TESTS ( -- )   \ a new device tag is a fresh baseline: never cross-tag compared
   PERF:RESET
   s" MMN" s" dev-15w" 1000 CT-ROW-DEV+       \ same kernel+config, old tag
   s" MMN" s" dev-25w" 2000 CT-ROW-DEV+       \ same kernel+config, new tag
   0 1 PERF:KEY= TFALSE                       \ differing device -> not the same key
   1 PERF:BASELINE -1 T=                      \ new-tag row has no same-tag baseline
   1 PERF:ROW-VERDICT PERF:V-MISSING T=       \ so it is a fresh baseline, not a regression
   PERF:SCAN 0 T=                             \ the cross-tag pair fabricates no regression
   s" MMN" s" dev-25w" 500 CT-ROW-DEV+        \ within-tag drop 2000 -> 500
   2 PERF:BASELINE 1 T=                       \ baseline is the earlier same-tag row, not the 15w row
   2 PERF:ROW-VERDICT PERF:V-REGRESS T=       \ within-tag regression is still caught
   PERF:SCAN 1 T= ;                           \ exactly one within-tag regression, no cross-tag noise

T-RESET
CT-VERDICT-TESTS
CT-MISSING-TESTS
CT-IMPROVE-TESTS
CT-REGRESS-TESTS
CT-EDGE-TESTS
CT-KEY-TESTS
CT-WAIVER-TESTS
CT-DEVICE-ISO-TESTS
T-REPORT

;package
