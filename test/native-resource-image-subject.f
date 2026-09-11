\ Native resources are used before capture and reacquired by the restored image.
require lib/f64-text.f
require lib/zip.f
require lib/test.f

package NATIVE-RESOURCE-SUBJECT

create DECIMAL F64-TEXT:MAX-BYTES allot
TYPED-VARIABLE SAVED-ARCHIVE ZIP:archive

: CHECK-F64 ( -- )
   s" 1.5" F64-TEXT:PARSE MATCH result
      ok OF IEEE754:F64>BITS $3FF8000000000000 T= ENDOF
      err OF drop -9899 throw ENDOF
   ;MATCH
   1.5 DECIMAL F64-TEXT:MAX-BYTES F64-TEXT:FORMAT MATCH result
      ok OF DECIMAL swap s" 1.5" T$= ENDOF
      err OF drop -9899 throw ENDOF
   ;MATCH ;

: CHECK-ZIP ( ZIP:archive -- ) {: archive:ZIP:archive :}
   archive ZIP:COUNT 5 T=
   archive archive 0 ZIP:ENTRY ZIP:READ s" stored value" T$=
   archive archive 1 ZIP:ENTRY ZIP:READ {: data:ptr bytes:n :}
   bytes 340 T= data 17 s" compressed value " T$= ;

: STALE-COUNT ( -- ) SAVED-ARCHIVE @ ZIP:COUNT drop ;
: STALE-CLOSE ( -- ) SAVED-ARCHIVE @ ZIP:CLOSE ;

public

\ Leave a live archive for the real image-capture callback to retire.
: WARM ( ptr u8 n -- )
   T-RESET CHECK-F64
   ZIP:OPEN dup SAVED-ARCHIVE ! CHECK-ZIP T-REPORT ;

: RUN ( ptr u8 n -- ) {: path:ptr bytes:n :}
   T-RESET
   [: STALE-COUNT ;] ZIP:E-HANDLE TTHROWSQ
   [: STALE-CLOSE ;] ZIP:E-HANDLE TTHROWSQ
   CHECK-F64
   path bytes ZIP:OPEN {: archive:ZIP:archive :}
   archive CHECK-ZIP archive ZIP:CLOSE
   T-REPORT ;

;package
