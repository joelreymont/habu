\ The engine evaluation boundary retains the explicit caller's stack contract.
require lib/test.f

package NATIVE-EVAL-TEST

TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: EV-N ( ptr u8 n -- n ) evaluate ;
TRUSTED: EV1 ( n ptr u8 n -- n ) evaluate ;

: DECLARE ( -- )
   s" : NATIVE-EVAL-ANSWER ( -- n ) 42 ;" EV ;

: RUN ( -- )
   T-RESET
   DECLARE
   s" NATIVE-EVAL-ANSWER" EV-N 42 T=
   41 s" 1+" EV1 42 T=
   s" BAD-EVAL ( ptr u8 n -- ) evaluate" CHECK! 0 T=
   T-REPORT ;

' RUN
;package
execute
