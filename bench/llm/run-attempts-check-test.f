\ run-attempts-check-test.f - checker-safe attempt runner smoke.
\
\ Load after bench/llm/run-attempts-lib.f with diagnostic-json-check-stub.f.

: RACT-MAIN ( -- )
   T-RESET
   RA-ROW-RESET
   RA-CHECKERS 0 T=
   RA-REPAIRS 0 T=
   RA-FIRST-CHECKER$ s" rejected" T$=
   RA-FIRST-TESTS? TFALSE
   RA-TESTS-PASSED? TFALSE
   RA-SIGNATURE-WEAKENED? TFALSE
   RA-ALL-ERRORS-STABLE? TTRUE
   RA-CHECKER++
   RA-CHECKERS 1 T=
   RA-REPAIRS 0 T=
   s" candidate.f" RA-SET-FINAL
   RA-FINAL$ s" candidate.f" T$=
   T-REPORT
   s" run-attempts-check-test: ok" type cr ;

RACT-MAIN
