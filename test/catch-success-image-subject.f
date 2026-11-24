\ Guarded catch source a --repl build links into hb, run as its MAIN.

require src/core/exit-status.f

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
   1 GUARDED 42 <> if s" wrong catch success" EXIT-STATUS:FAILURE die then
   [: -1 GUARDED drop ;] catch -71 <> if s" wrong catch failure" EXIT-STATUS:FAILURE die then
   s" catch-image: ok" type cr ;
;package

: MAIN ( -- ) CATCH-IMAGE:RUN ;
