\ A conditional loop carries two six-cell values through calls and an early
\ exit. Spill rewriting must reuse a frame-order argument whose consumer is
\ beyond a conditional edge; duplicating it fails allocation with E-A64RA-EDGE.
\ Tier 1 first: the reused frame-order argument - duplicating it fails with
\ E-A64RA-EDGE - is the optimizing compiler's spill rewriting.
1 set-tier

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

\ The early return and loop body share a frame-order class after spilling.
\ Their tokens overlap in linear block order but occur on exclusive CFG paths.
package NATIVE-ORDER-TEST
private
variable TOTAL
: AR-HEAD ( n n -- n ) + ;
: AR-ROW ( n n -- n ) + ;
: AR-COUNT ( n -- n ) 3 and ;
: AR-COLUMN ( n -- n ) 7 and ;
: AR-SPAN ( n -- n ) 1 and 1+ ;
: AR-SAVE ( n n n n n n n n -- ) + + + + + + + TOTAL ! ;
: SCAN ( n n n n n n n n -- )
   {: found:n pattern:n panel:n heading:n first:n at:n column:n given:n :}
   heading first column + AR-HEAD {: title:n :}
   panel at AR-ROW {: native:n :}
   given {: height:n :}
   height 0= if exit then
   height 1 = if
      native AR-COUNT 0 ?do
         native i + {: entry:n :}
         entry AR-COLUMN title AR-COLUMN = entry AR-SPAN title AR-SPAN = and if
            unloop exit
         then
      loop
   then
   found pattern panel heading first at title height AR-SAVE ;

public
: CASES ( -- )
   s" scalar values survive the conditional loop and calls" T-LABEL
   -999 TOTAL !
   1 2 3 4 5 0 1 1 SCAN TOTAL @ 26 T=
   s" the guarded path skips the loop and keeps all values" T-LABEL
   -999 TOTAL !
   1 2 3 4 5 0 1 2 SCAN TOTAL @ 27 T=
   s" zero iterations preserve the outgoing values" T-LABEL
   -999 TOTAL !
   1 2 4 4 5 0 1 1 SCAN TOTAL @ 27 T=
   s" early exit before the loop leaves the result untouched" T-LABEL
   -999 TOTAL !
   1 2 3 4 5 0 1 0 SCAN TOTAL @ -999 T=
   s" early exit inside the loop leaves the result untouched" T-LABEL
   -999 TOTAL !
   1 2 3 1 1 0 1 1 SCAN TOTAL @ -999 T= ;
;package
\ An unchanged header argument needs no backedge copy. It can remain live past
\ that edge in block layout because UNTIL's returning path follows its stub.
package NATIVE-UNTIL-TEST
private
: NEXT ( n -- n ) 1+ ;
: RETURN-CARRY ( n n -- n )
   >r begin NEXT dup 1000 > until r> + ;
: LOCAL-CARRY ( n n -- n )
   {: kept:n start:n :}
   start begin NEXT dup 1000 > until kept + ;
: TWO-CARRIERS ( n n n -- n )
   >r >r begin NEXT dup 1000 > until r> + r> + ;
\ The unchanged return-stack carrier and both changing stack lanes share the
\ same backedge. The latter still need parallel copies before their writes.
: PERMUTE-CARRY ( n n n n -- n )
   >r begin NEXT >r swap r> dup 3 >= until drop - r> + ;

public
: CASES ( -- )
   s" an unchanged return-stack value survives repeated UNTIL calls" T-LABEL
   0 42 RETURN-CARRY 1043 T=
   s" the first UNTIL exit preserves its return-stack value" T-LABEL
   1000 57 RETURN-CARRY 1058 T=
   s" an unchanged local survives the UNTIL backedge and call" T-LABEL
   42 0 LOCAL-CARRY 1043 T=
   s" distinct unchanged return-stack lanes survive UNTIL" T-LABEL
   0 31 42 TWO-CARRIERS 1074 T=
   s" an unchanged lane does not break odd backedge permutations" T-LABEL
   1 9 0 42 PERMUTE-CARRY 50 T=
   s" even backedge permutations preserve the original lane order" T-LABEL
   1 9 1 42 PERMUTE-CARRY 34 T= ;
;package
T-RESET NSP:CASES NATIVE-ORDER-TEST:CASES NATIVE-UNTIL-TEST:CASES T-REPORT
