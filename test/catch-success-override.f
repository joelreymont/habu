\ Replace a previously certified zero-test summary after restoring its image.
1 set-tier
package CATCH-IMAGE
public
undefine ZERO?
: ZERO? ( n -- bool ) drop true ;
variable OVERRIDE-FAIL
: OVERRIDE-NEW ( -- n ) OVERRIDE-FAIL @ 0<> if 84 throw then 31 ;
: OVERRIDE-READER ( n -- n ) drop OVERRIDE-NEW ;
: UNSAFE ( n -- n )
   [: OVERRIDE-READER ;] catch {: value code:n :}
   code ZERO? if value 1+ else 0 then ;
: EXERCISE ( -- ) 1 UNSAFE drop ;
;package

CATCH-IMAGE:EXERCISE
