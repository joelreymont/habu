\ negative-score.f - checked scorer for benchmark negative-harness rows.
\
\ Load after lib/errors.f and lib/string.f.

59 constant NS-SEMI
-3206 constant E-BM-NEGATIVE-META

0 constant NS-CORRECT
1 constant NS-SILENT-SUCCESS
2 constant NS-WRONG-OUTCOME
3 constant NS-MISSING-DIAG
4 constant NS-WRONG-CODE
5 constant NS-WRONG-TOKEN
6 constant NS-WRONG-CLASS

variable NS-NEXT

: NS-TRUE ( -- bool )
   0 0= ;

: NS-FALSE ( -- bool )
   NS-TRUE 0= ;

: NS-OUTCOME-PASS? ( ptr u8 n -- bool )
   s" pass" STR= ;

: NS-OUTCOME-REJECT? ( ptr u8 n -- bool )
   s" reject" STR= ;

: NS-KEYED? ( ptr u8 n ptr u8 n -- bool ) {: item:ptr itemu key:ptr keyu :}
   itemu keyu <= if NS-FALSE exit then
   item keyu key keyu STR= 0= if NS-FALSE exit then
   item keyu + c@ STR-SPACE = ;

: NS-ITEM-VALUE$ ( ptr u8 n n -- ptr u8 n ) {: item:ptr itemu keyu :}
   item keyu + itemu keyu - TRIM ;

: NS-FIELD$ ( ptr u8 n ptr u8 n -- ptr u8 n bool ) {: meta:ptr metau key:ptr keyu :}
   0 NS-NEXT !
   begin
      meta metau NS-SEMI NS-NEXT @ SPLIT-NEXT
   while
      NS-NEXT !
      TRIM 2dup key keyu NS-KEYED? if
         keyu NS-ITEM-VALUE$ NS-TRUE exit
      then
      2drop
   repeat
   drop 2drop
   key 0 NS-FALSE ;

: NS-REQUIRE-CODE ( ptr u8 n -- )
   s" code" NS-FIELD$ if 2drop exit then
   2drop E-BM-NEGATIVE-META throw ;

: NS-CODE$ ( ptr u8 n -- ptr u8 n ) {: meta:ptr metau :}
   meta metau NS-REQUIRE-CODE
   meta metau s" code" NS-FIELD$ drop ;

: NS-DIAG-CONTAINS? ( ptr u8 n ptr u8 n -- bool ) {: diag:ptr diagu needle:ptr needleu :}
   needleu 0= if NS-TRUE exit then
   diag diagu needle needleu CONTAINS? ;

: NS-REQUIRED-CODE-OK? ( ptr u8 n ptr u8 n -- bool ) {: meta:ptr metau diag:ptr diagu :}
   diag diagu meta metau NS-CODE$ NS-DIAG-CONTAINS? ;

: NS-OPTIONAL-FIELD-OK? ( ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: meta:ptr metau key:ptr keyu diag:ptr diagu :}
   meta metau key keyu NS-FIELD$ if
      diag diagu 2swap NS-DIAG-CONTAINS? exit
   then
   2drop NS-TRUE ;

: NS-TOKEN-OK? ( ptr u8 n ptr u8 n -- bool ) {: meta:ptr metau diag:ptr diagu :}
   meta metau s" token" diag diagu NS-OPTIONAL-FIELD-OK? ;

: NS-CLASS-OK? ( ptr u8 n ptr u8 n -- bool ) {: meta:ptr metau diag:ptr diagu :}
   meta metau s" class" diag diagu NS-OPTIONAL-FIELD-OK? ;

: NS-SCORE ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: meta:ptr metau outcome:ptr outcomeu diag:ptr diagu :}
   outcome outcomeu NS-OUTCOME-PASS? if NS-SILENT-SUCCESS exit then
   outcome outcomeu NS-OUTCOME-REJECT? 0= if NS-WRONG-OUTCOME exit then
   diagu 0= if NS-MISSING-DIAG exit then
   meta metau diag diagu NS-REQUIRED-CODE-OK? 0= if NS-WRONG-CODE exit then
   meta metau diag diagu NS-TOKEN-OK? 0= if NS-WRONG-TOKEN exit then
   meta metau diag diagu NS-CLASS-OK? 0= if NS-WRONG-CLASS exit then
   NS-CORRECT ;
