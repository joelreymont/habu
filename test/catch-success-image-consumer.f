split-str
\ Fresh guarded source compiled against the zero-test summary the built hb carries.

require src/core/exit-status.f

package CATCH-IMAGE
public
variable FRESH-FAIL
: FRESH-NEW ( -- n ) FRESH-FAIL @ 0<> if -71 throw then 41 ;
: FRESH-READER ( n -- n ) drop FRESH-NEW ;
: FRESH-GUARD ( n -- n )
   [: FRESH-READER ;] catch {: code:n :}
   code ZERO? if 1+ else drop code throw then ;
: FRESH-RUN ( -- )
   0 FRESH-FAIL !
   0 FRESH-GUARD 42 <> if s" wrong fresh success" EXIT-STATUS:FAILURE die then
   1 FRESH-FAIL !
   [: 0 FRESH-GUARD drop ;] catch -71 <> if s" wrong fresh failure" EXIT-STATUS:FAILURE die then
   s" catch-image-fresh: ok" type cr ;
;package

CATCH-IMAGE:FRESH-RUN
