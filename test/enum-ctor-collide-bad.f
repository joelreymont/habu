\ A generated constructor collision reports a catchable declaration error.
require lib/test.f

package ENUM-CTOR-COLLIDE-BAD

TRUSTED: EV ( ptr u8 n -- ) evaluate ;

: ECOL:RED ( -- n ) 37 ;
: DECLARE ( -- ) s" ENUM-DECL:ED-RUN ecol green red ;ENUM" EV ;

public

\ Run outside the package so the declaration publishes public constructors.
: MAIN ( -- )
   T-RESET
   [: DECLARE ;] TYPE-DECL:E-TDECL-NAME TTHROWSQ
   GENERATED-DECL:DEPTH 0 T=
   ECOL:RED 37 T=
   T-REPORT ;

;package

ENUM-CTOR-COLLIDE-BAD:MAIN
