\ A conditional loop carries two six-cell values through calls and an early
\ exit. Spill rewriting must reuse a frame-order argument whose consumer is
\ beyond a conditional edge; duplicating it fails allocation with E-A64RA-EDGE.
require lib/test.f
package NSP
public
ENUM verdict 0 DERIVE eq
   VARIANT clear ;VARIANT
   VARIANT collision ;VARIANT
;ENUM
SUMTYPE shape 0
   VARIANT core n n n n n ;VARIANT
;SUMTYPE

private

: ITEM-SPAN-MASK ( n -- n ) 7 and ;
: SHAPE-ON ( n shape n -- shape ) {: item:n shape bit:n :} shape ;
: PAIR-CLEARANCE ( n n -- n ) max ;
: SHAPE-TOTAL ( shape -- n )
   MATCH shape
      core OF {: a:n b:n c:n d:n e:n :}
         a 10000 * b 1000 * + c 100 * + d 10 * + e +
      ENDOF
   ;MATCH ;
: SEPARATED? ( shape shape n -- bool ) {: a b clearance:n :}
   a SHAPE-TOTAL b SHAPE-TOTAL + clearance = ;
: SHAPED-PAIR ( n shape n shape -- verdict ) {: a:n sa b:n sb :}
   a ITEM-SPAN-MASK b ITEM-SPAN-MASK and {: shared:n :}
   2 0 ?do
      1 i lshift {: bit:n :}
      shared bit and 0<> if
         a sa bit SHAPE-ON b sb bit SHAPE-ON a b PAIR-CLEARANCE SEPARATED? 0= if
            NSP-VERDICT:COLLISION unloop exit
         then
      then
   loop NSP-VERDICT:CLEAR ;

public

: CASES ( -- )
   s" disjoint masks skip both loop body calls" T-LABEL
   1 0 0 0 0 0 NSP-SHAPE:CORE
   2 0 0 0 0 0 NSP-SHAPE:CORE SHAPED-PAIR
   NSP-VERDICT:CLEAR NSP-VERDICT:EQ TTRUE
   s" a collision exits through the first active loop edge" T-LABEL
   1 0 0 0 0 0 NSP-SHAPE:CORE
   3 0 0 0 0 0 NSP-SHAPE:CORE SHAPED-PAIR
   NSP-VERDICT:COLLISION NSP-VERDICT:EQ TTRUE
   s" a collision exits after skipping the first layer" T-LABEL
   2 0 0 0 0 0 NSP-SHAPE:CORE
   3 0 0 0 0 0 NSP-SHAPE:CORE SHAPED-PAIR
   NSP-VERDICT:COLLISION NSP-VERDICT:EQ TTRUE
   s" all wide payload fields survive both loop iterations" T-LABEL
   3 1 2 3 4 5 NSP-SHAPE:CORE
   66667 5 4 3 2 2 NSP-SHAPE:CORE SHAPED-PAIR
   NSP-VERDICT:CLEAR NSP-VERDICT:EQ TTRUE ;
;package
T-RESET NSP:CASES T-REPORT
