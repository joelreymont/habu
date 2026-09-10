\ Tail-call liveness must read the function currently being selected.
require lib/test.f

package TAIL-OWNER-TEST
public

NEWTYPE length 0
CAST: >LENGTH ( n -- length )
CAST: LENGTH>N ( length -- n )
PRODUCT point 0
   FIELD x length
   FIELD y length
;PRODUCT


: VALIDATE ( length -- )
   LENGTH>N dup -1000000000 < swap 1000000000 > or
   if 73 throw then ;


: POINT ( length length -- point )
   {: x:length y:length :}
   x VALIDATE y VALIDATE
   x y TAIL--OWNER--TEST-POINT:MAKE ;


\ Compile a second function after another module has been emitted. Its final
\ call carries two locals whose unused results need the tail liveness scan.
: OTHER-POINT ( length length -- point )
   {: x:length y:length :}
   y VALIDATE x VALIDATE
   y x TAIL--OWNER--TEST-POINT:MAKE ;


: RUN ( -- )
   T-RESET
   17 >LENGTH 29 >LENGTH POINT TAIL--OWNER--TEST-POINT:UNMAKE
   LENGTH>N 29 T= LENGTH>N 17 T=
   17 >LENGTH 29 >LENGTH OTHER-POINT TAIL--OWNER--TEST-POINT:UNMAKE
   LENGTH>N 17 T= LENGTH>N 29 T=
   T-REPORT ;

RUN
;package
