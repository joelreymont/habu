\ workspace.f - CWORK storage: the declaring package's own words capture none
\ of its accessors, and no checked source reaches the mapping's cells.

require src/lib/test.f
require src/lib/fmt.f
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

package CWKT
public
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

\ A 1-byte row stored as the mapping or as a buffer's offset would hand out
\ typed pointers past it.
create CWK-FOOB 1 allot

\ The text `value CWORK-VARIABLE <name><i> n`.
: CWK-SRC$ ( n ptr u8 n n -- ptr u8 n ) {: value:n na:ptr nu:n i:n :}
   SB-RESET  value FMT:SB-INT  s"  CWORK-VARIABLE " SB-APPEND
   na nu SB-APPEND  i FMT:SB-INT  s"  n" SB-APPEND  SB$ ;

\ Declares CWK-Pi with no default, then CWK-Di with one; answers the code
\ CWK-Di's declaration threw.
: CWK-TURN ( n -- n ) {: i:n :}
   0 s" CWK-P" i CWK-SRC$ TEST-EVAL:RC 0 T=
   1 s" CWK-D" i CWK-SRC$ TEST-EVAL:RC ;

\ Turns from 0 until a CWK-Di is refused; answers that i and its code.
: CWK-FILL ( -- n n )
   0 begin dup CWK-TURN dup 0= while drop 1+ repeat ;

\ The bytes from CWK-Pi to CWK-A.
: CWK-GAP ( n -- n ) {: i:n :}
   SB-RESET  s" CWK-A NULL-PTR - CWK-P" SB-APPEND
   i FMT:SB-INT  s"  NULL-PTR - -" SB-APPEND  SB$ TEST-EVAL:N ;
;package

CWKT:CWK-V T=
CWKC:V @ -5 T=
CWKT:CWK-B T=
2 CWKC:B @ 3 T=
CWKT:CWK-S T=
CWKW:HITS @ 0 T=

s" : CWK-MEM ( -- ) CWKT:CWK-FOOB CWORK:MEM#base ! ;" TEST-EVAL:RC CHECKER-REJECT-RC T=
s" : CWK-CTL ( -- ) 0 CWORK:MEM#ctl ! ;" TEST-EVAL:RC CHECKER-REJECT-RC T=
s" : CWK-OFF ( -- ) 0 CWKC:V#base ! ;" TEST-EVAL:RC CHECKER-REJECT-RC T=
s" 0 CWORK-VARIABLE CWK-U8 u8" TEST-EVAL:RC CHECKER-REJECT-RC T=

\ Defaults fill the table; the refused declaration lays nothing, so the
\ variables declared around it are one cell apart.
CWKT:CWK-FILL E-LAYOUT-BUFFER T=
s" 0 CWORK-VARIABLE CWK-A n" TEST-EVAL:RC 0 T=
CWKT:CWK-GAP CELL T=

T-REPORT
