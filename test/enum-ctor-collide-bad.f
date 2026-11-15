\ A generated constructor collision prints its rejection record and throws the
\ checker's status, which the caller catches.
require src/lib/test.f

package ENUM-CTOR-COLLIDE-BAD

: ECOL:RED ( -- n ) 37 ;
: DECLARE ( -- ) s" ENUM ecol green red ;ENUM" evaluate-closed ;

public

\ Run outside the package so the declaration publishes public constructors.
: MAIN ( -- )
   T-RESET
   [: DECLARE ;] CHECKER-REJECT-RC TTHROWSQ
   ECOL:RED 37 T=
   T-REPORT ;

;package

ENUM-CTOR-COLLIDE-BAD:MAIN
