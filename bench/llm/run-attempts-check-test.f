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
   s" run" 1 s" NAME" s" model" 0 RA-ROW$ {: row:ptr rowu :}
   row rowu s" run_id" CONTAINS? TTRUE
   row rowu s" repair_class_stats" CONTAINS? TTRUE
   row rowu s" signature_weakened" CONTAINS? TTRUE
   T-REPORT
   s" run-attempts-check-test: ok" type cr ;

RACT-MAIN
