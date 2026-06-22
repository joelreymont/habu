\ fixture-text.f - checked text fixture builder vocabulary.
\
\ Load after lib/string.f, lib/memory.f, and lib/json-write.f.

9 constant BFT-TAB
10 constant BFT-LF
8 constant BFT-BS
12 constant BFT-FF
13 constant BFT-CR
92 constant BFT-BACKSLASH
127 constant BFT-DEL

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

: BFT-JSON-ESCAPE-SAMPLE$ ( -- ptr u8 n )
   BFT-RESET
   s" a" BFT+
   BFT-DQ+
   s" b" BFT+
   BFT-BACKSLASH BFT-C
   s" c" BFT+
   BFT-BS BFT-C
   BFT-FF BFT-C
   BFT-LF BFT-C
   BFT-CR BFT-C
   BFT-TAB BFT-C
   0 BFT-C
   1 BFT-C
   BFT-DEL BFT-C
   BFT$ ;

: BFT-JSON-STRING$ ( ptr u8 n -- ptr u8 n )
   JW-RESET
   JW-STRING
   JW$ ;
