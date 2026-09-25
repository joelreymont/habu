\ Guarded catch source captured into an application image.
package CATCH-IMAGE
private
variable FAIL
: NEW-VALUE ( -- n ) FAIL @ 0<> if -71 throw then 41 ;
: READER ( n -- n ) drop NEW-VALUE ;
public
: ZERO? ( n -- bool ) 0= ;
: GUARDED ( n -- n )
   dup 0< if 1 else 0 then FAIL !
   [: READER ;] catch {: code:n :}
   code ZERO? if 1+ else drop code throw then ;
: RUN ( -- )
   1 GUARDED 42 <> if s" wrong catch success" 74 die then
   [: -1 GUARDED drop ;] catch -71 <> if s" wrong catch failure" 74 die then
   s" catch-image: ok" type cr ;
;package
