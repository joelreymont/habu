\ test-test.f - focused tests for lib/test.f.
\ Run: cat lib/test.f lib/test-test.f | bin/hb

: TT-THROW-7 ( -- )
   7 throw ;

T-RESET

1 1 T=
2 3 T<>
-1 TTRUE
0 TFALSE
s" alpha" s" alpha" T$=
s" alpha" s" beta" T$<>
' TT-THROW-7 7 TTHROWS

T-CASES 7 T=
T-FAILURES 0 T=
T-REPORT
