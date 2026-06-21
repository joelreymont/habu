\ fixture-text-test.f - tests for checked text fixture builders.
\
\ Load after lib/test.f and bench/llm/fixture-text.f.

: BFTT-ASSERT-SOURCE-S" ( -- )
   BFT-RESET
   s" alpha" BFT-SOURCE-S"
   BFT$ {: a:ptr u :}
   u 9 T=
   a c@ 115 T=
   a 1 + c@ JW-DQ T=
   a 2 + c@ JW-SP T=
   a 8 + c@ JW-DQ T=
   a 3 + 5 s" alpha" T$= ;

: BFTT-ASSERT-SOURCE-DEF ( -- )
   BFT-RESET
   s" MAIN" s" --" BFT-SOURCE-DEF
   s" 42 " BFT+
   BFT-SOURCE-END$ s" : MAIN ( -- ) 42 ;" T$= ;

: BFTT-ASSERT-TSV ( -- )
   BFT-RESET
   s" a" BFT-TSV-CELL
   BFT-TSV-BLANK
   s" c" BFT-TSV-LAST
   BFT$ {: a:ptr u :}
   u 5 T=
   a c@ 97 T=
   a 1 + c@ BFT-TAB T=
   a 2 + c@ BFT-TAB T=
   a 3 + c@ 99 T=
   a 4 + c@ BFT-LF T= ;

: BFTT-MAIN ( -- )
   T-RESET
   BFTT-ASSERT-SOURCE-S"
   BFTT-ASSERT-SOURCE-DEF
   BFTT-ASSERT-TSV
   T-REPORT
   s" fixture-text-test: ok" type cr ;

BFTT-MAIN
