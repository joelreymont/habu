\ workspace.f - CWORK storage: the declaring package's own words capture none
\ of its accessors, and no checked source reaches the mapping's cells.

require src/lib/string.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require src/lib/fmt.f
require src/compiler/workspace.f

T-RESET

package CWKW
public
variable HITS
private
: HIT ( -- ) 1 HITS +! ;
: @ ( ^n -- n ) HIT drop 0 ;
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
: CWK-CELLS ( [ n -- ^n ] -- n ) {: at :}
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
: CWK-SRC$ ( n str n -- str ) {: value:n ns:str i:n :}
   SB-RESET  value FMT:SB-INT  s"  CWORK-VARIABLE " SB-APPEND
   ns SB-APPEND  i FMT:SB-INT  s"  n" SB-APPEND  SB$ ;

\ Declares CWK-Di with a default for more i than the default table holds.
: CWK-FILL ( -- )
   $101 0 ?do 1 s" CWK-D" i CWK-SRC$ TEST-EVAL:RC drop loop ;

$4000 constant CWK-CAP
create CWK-OUT CWK-CAP allot
create CWK-ERR CWK-CAP allot

\ A full default table ends the process, so CWK-FILL runs in a child.
: CWK-FULL ( -- )
   s" CWKT:CWK-FILL" {: src:str :}
   src CWK-OUT CWK-CAP >LEN CWK-ERR CWK-CAP >LEN PROCESS:WAIT-FOREVER
   SUBJECT:RUN {: outl:len errl:len oc :}
   CWK-ERR errl LEN>N >str {: err:str :}
   src CWK-OUT outl LEN>N >str err oc 90 T-ENDING-EXITED=
   err
   s" compiler workspace: the default table is full at DEFAULT-CAP rows"
   STR:CONTAINS? TTRUE ;
;package

CWKT:CWK-V T=
CWKC:V @ -5 T=
CWKT:CWK-B T=
2 CWKC:B @ 3 T=
CWKT:CWK-S T=
CWKW:HITS @ 0 T=

s" : CWK-MEM ( -- ) CWKT:CWK-FOOB CWORK:MEM#base ! ;" TEST-EVAL:RC 70 T=
s" : CWK-CTL ( -- ) 0 CWORK:MEM#ctl ! ;" TEST-EVAL:RC 70 T=
s" : CWK-OFF ( -- ) 0 CWKC:V#base ! ;" TEST-EVAL:RC 70 T=
s" 0 CWORK-VARIABLE CWK-U8 u8" TEST-EVAL:RC 70 T=

CWKT:CWK-FULL

T-REPORT
