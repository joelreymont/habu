\ test-test.f - focused tests for lib/test.f.
\ Run: bin/hb --load lib/test.f lib/test-test.f

: TT-THROW-7 ( -- )
   7 throw ;

: TT-THROW-5 ( -- )
   5 throw ;

T-RESET
s" numeric mismatch" T-LABEL
1 2 T=
T-CASES 1 T=
T-FAILURES 1 T=

T-RESET
T-FAIL+
T-FAILURES 1 T=

T-RESET
' TT-THROW-5 4 TTHROWS
T-CASES 1 T=
T-FAILURES 1 T=

T-RESET

1 1 T=
2 3 T<>
-1 TTRUE
0 TFALSE
s" alpha" s" alpha" T$=
s" alpha" s" beta" T$<>
' TT-THROW-7 7 TTHROWS
T-LABEL$ s" " T$=
s" alpha-label" T-LABEL
T-LABEL$ s" alpha-label" T$=
T-LABEL$ s" " T$=
s" clear-label" T-LABEL
T-LABEL-CLEAR
T-LABEL$ s" " T$=
s" true label" T-LABEL
-1 TTRUE
T-LABEL$ s" " T$=

T-CASES 13 T=
T-FAILURES 0 T=
T-REPORT
