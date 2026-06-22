\ run-test.f - focused tests for the native benchmark gate runner.
\
\ Load after lib/test.f and bench/llm/run-lib.f.

: BGRT-ARGV-BASE ( -- )
   BGR-HB
   PROC-ARGV-N @ COUNT>N 1 T= ;

: BGRT-SMOKE ( -- )
   BGR-JSON-ROW ;

: BGRT-MAIN ( -- )
   T-RESET
   BGRT-ARGV-BASE
   BGRT-SMOKE
   T-REPORT
   s" run-test: ok" type cr ;

BGRT-MAIN
