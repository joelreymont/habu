\ workspace.f - CWORK storage: the declaring package's own words capture none
\ of its accessors, and no checked source reaches the mapping's cells.

require src/lib/test.f
require src/compiler/workspace.f

T-RESET

package CWKW
public
variable HITS
private
: HIT ( -- ) 1 HITS +! ;
: @ ( ptr n -- n ) HIT drop 0 ;
: * ( n n -- n ) HIT + ;
: + ( a n -- a ) HIT drop ;
: swap ( a b -- b a ) {: x y :} HIT y x ;
: invert ( n -- n ) HIT ;
public
-5 CWORK-VARIABLE V n
3 CWORK-BUFFER B n
2 CWORK-SPAN S n
;package

package CWKC
public
-5 CWORK-VARIABLE V n
3 CWORK-BUFFER B n
2 CWORK-SPAN S n
;package

\ Stores 1, 2 and 3 at indices 0 to 2 and reads them back as one number.
: CWK-CELLS ( [ n -- ptr n ] -- n ) {: at :}
   3 0 ?do i 1 + i at execute ! loop
   0 3 0 ?do 10 * i at execute @ + loop ;

\ Each answers CWKW's then CWKC's.
: CWK-B ( -- n n ) [: CWKW:B ;] CWK-CELLS  [: CWKC:B ;] CWK-CELLS ;
: CWK-V ( -- n n ) CWKW:V @  CWKC:V @ ;
: CWK-S ( -- n n )
   7 CWKW:S !  8 CWKW:S cell+ !  7 CWKC:S !  8 CWKC:S cell+ !
   CWKW:S @ CWKW:S cell+ @ *  CWKC:S @ CWKC:S cell+ @ * ;

CWK-V T=
CWKC:V @ -5 T=
CWK-B T=
2 CWKC:B @ 3 T=
CWK-S T=
CWKW:HITS @ 0 T=

\ A 1-byte row stored as the mapping or as a buffer's offset would hand out
\ typed pointers past it.
create CWK-FOOB 1 allot
s" : CWK-MEM ( -- ) CWK-FOOB CWORK:MEM#base ! ;" TEST-EVAL:RC CHECKER-REJECT-RC T=
s" : CWK-CTL ( -- ) 0 CWORK:MEM#ctl ! ;" TEST-EVAL:RC CHECKER-REJECT-RC T=
s" : CWK-OFF ( -- ) 0 CWKC:V#base ! ;" TEST-EVAL:RC CHECKER-REJECT-RC T=

T-REPORT
