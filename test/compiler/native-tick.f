\ A named quotation retains its effect and native calling convention.
require lib/test.f

package NATIVE-TICK-TEST

: INC ( n -- n ) 1+ ;
defer HOOK ( n -- n )
variable CALLED
: COUNT-HOOK ( -- ) 1 CALLED +! ;

: REBIND-CAPTURED ( -- )
   0 CALLED !
   ['] COUNT-HOOK is A64RAV:DKEEP-HOOK
   A64RAV:DKEEP-HOOK
   A64RAV:DKEEP-HOOK-DEFAULT
   CALLED @ 1 T= ;

: INSTALL ( -- )
   ['] INC is HOOK ;

: CALL ( n -- n )
   ['] INC execute ;

: RUN ( -- )
   T-RESET
   REBIND-CAPTURED
   INSTALL
   41 HOOK 42 T=
   41 CALL 42 T=
   T-REPORT ;

RUN
;package
