\ fixture-text.f - checked text fixture builder vocabulary.
\
\ Load after lib/string.f and lib/json-write.f.

9 constant BFT-TAB
10 constant BFT-LF

: BFT-RESET ( -- )
   SB-RESET ;

: BFT$ ( -- ptr u8 n )
   SB$ ;

: BFT+ ( ptr u8 n -- )
   SB-APPEND ;

: BFT-C ( n -- )
   SB-APPEND-C ;

: BFT-TAB+ ( -- )
   BFT-TAB BFT-C ;

: BFT-LF+ ( -- )
   BFT-LF BFT-C ;

: BFT-DQ+ ( -- )
   JW-DQ BFT-C ;

: BFT-SP+ ( -- )
   JW-SP BFT-C ;

: BFT-SOURCE-S" ( ptr u8 n -- ) {: a:ptr u :}
   s" s" BFT+
   BFT-DQ+
   BFT-SP+
   a u BFT+
   BFT-DQ+ ;

: BFT-SOURCE-DEF ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu sig:ptr sigu :}
   s" : " BFT+
   name nameu BFT+
   s"  ( " BFT+
   sig sigu BFT+
   s"  ) " BFT+ ;

: BFT-SOURCE-END$ ( -- ptr u8 n )
   s" ;" BFT+
   BFT$ ;

: BFT-TSV-CELL ( ptr u8 n -- )
   BFT+
   BFT-TAB+ ;

: BFT-TSV-BLANK ( -- )
   BFT-TAB+ ;

: BFT-TSV-LAST ( ptr u8 n -- )
   BFT+
   BFT-LF+ ;
