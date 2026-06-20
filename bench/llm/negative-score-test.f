\ negative-score-test.f - focused tests for bench/llm/negative-score.f.

: NST-AOT-META$ ( -- ptr u8 n )
   s" code E-AOT-UNSUPPORTED; token here" ;

: NST-RX-META$ ( -- ptr u8 n )
   s" code E-RX-SYNTAX" ;

: NST-CLASS-META$ ( -- ptr u8 n )
   s" code E-CHECK; class trusted_boundary_required" ;

: NST-AOT-DIAG$ ( -- ptr u8 n )
   s" code E-AOT-UNSUPPORTED token here verdict rejected" ;

: NST-RX-DIAG$ ( -- ptr u8 n )
   s" throw E-RX-SYNTAX while compiling pattern" ;

: NST-CLASS-DIAG$ ( -- ptr u8 n )
   s" code E-CHECK repair_class trusted_boundary_required" ;

: NST-EXPECT-BAD-META ( -- )
   s" token here" s" reject" NST-AOT-DIAG$ NS-SCORE drop ;

: NS-TEST-MAIN ( -- )
   T-RESET
   NST-AOT-META$ s" code" NS-FIELD$ TTRUE s" E-AOT-UNSUPPORTED" T$=
   NST-AOT-META$ s" token" NS-FIELD$ TTRUE s" here" T$=
   NST-AOT-META$ s" class" NS-FIELD$ TFALSE 2drop
   NST-AOT-META$ s" reject" NST-AOT-DIAG$ NS-SCORE NS-CORRECT T=
   NST-RX-META$ s" reject" NST-RX-DIAG$ NS-SCORE NS-CORRECT T=
   NST-CLASS-META$ s" reject" NST-CLASS-DIAG$ NS-SCORE NS-CORRECT T=
   NST-AOT-META$ s" pass" NST-AOT-DIAG$ NS-SCORE NS-SILENT-SUCCESS T=
   NST-AOT-META$ s" error" NST-AOT-DIAG$ NS-SCORE NS-WRONG-OUTCOME T=
   NST-AOT-META$ s" reject" s" " NS-SCORE NS-MISSING-DIAG T=
   NST-AOT-META$ s" reject" s" code E-OTHER token here" NS-SCORE NS-WRONG-CODE T=
   NST-AOT-META$ s" reject" s" code E-AOT-UNSUPPORTED token allot" NS-SCORE NS-WRONG-TOKEN T=
   NST-CLASS-META$ s" reject" s" code E-CHECK repair_class remove_producer" NS-SCORE NS-WRONG-CLASS T=
   ['] NST-EXPECT-BAD-META E-BM-NEGATIVE-META TTHROWS
   T-REPORT
   s" negative-score-test: ok" type cr ;

NS-TEST-MAIN
