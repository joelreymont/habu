\ A generated constructor collision prints its rejection record and throws the
\ checker's status, which the caller catches.
require src/core/exit-status.f
require src/lib/test.f

package ENUM-CTOR-COLLIDE-BAD

: ECOL-RED ( -- n ) 37 ;
: DECLARE ( -- ) s" ENUM ecol green red ;ENUM" evaluate-closed ;

: MAIN ( -- )
   T-RESET
   [: DECLARE ;] EXIT-STATUS:REJECTED TTHROWSQ
   ECOL-RED 37 T=
   T-REPORT ;

\ Run in the private section, where the ENUM's variant red is the word ECOL-RED.
MAIN

;package
