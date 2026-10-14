\ Native resources are used before capture and reacquired by the restored image.
require src/lib/f64-text.f
require src/lib/zip.f
require src/lib/test.f

package NATIVE-RESOURCE-SUBJECT

create DECIMAL F64-TEXT:MAX-BYTES allot
create ZIP-BYTES $20 allot
TYPED-VARIABLE SAVED-ARCHIVE ZIP:archive
TYPED-VARIABLE SAVED-FILE ZIP:archive
TYPED-VARIABLE SAVED-INPUT ZIP:input

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
: STALE-FILE ( -- ) SAVED-FILE @ ZIP:COUNT drop ;
: STALE-INPUT ( -- ) SAVED-INPUT @ ZIP-BYTES 1 SPAN:MAKE ZIP:READ-IN drop ;

: CHECK-FILE ( ZIP:archive -- ) {: archive:ZIP:archive :}
   archive CHECK-ZIP
   archive archive 0 ZIP:ENTRY ZIP:OPEN-ENTRY {: input:ZIP:input :}
   input ZIP-BYTES $20 SPAN:MAKE ZIP:READ-IN 12 T=
   ZIP-BYTES 12 s" stored value" T$=
   input ZIP:CLOSE-ENTRY
   archive ZIP:OPEN-SOURCE {: source:ZIP:input :}
   source ZIP-BYTES 4 SPAN:MAKE ZIP:READ-IN 4 T=
   ZIP-BYTES 4 S\" \x50\x4B\x03\x04" T$=
   source ZIP:CLOSE-ENTRY ;

public

\ Leave a live archive for the real image-capture callback to retire.
: WARM ( ptr u8 n -- )
   T-RESET CHECK-F64
   2dup ZIP:OPEN dup SAVED-ARCHIVE ! CHECK-ZIP
   ZIP:OPEN-FILE dup SAVED-FILE !
   dup CHECK-FILE dup 0 ZIP:ENTRY ZIP:OPEN-ENTRY SAVED-INPUT !
   T-REPORT ;

: RUN ( ptr u8 n -- ) {: path:ptr bytes:n :}
   T-RESET
   [: STALE-COUNT ;] ZIP:E-HANDLE TTHROWSQ
   [: STALE-CLOSE ;] ZIP:E-HANDLE TTHROWSQ
   [: STALE-FILE ;] ZIP:E-HANDLE TTHROWSQ
   [: STALE-INPUT ;] ZIP:E-HANDLE TTHROWSQ
   CHECK-F64
   path bytes ZIP:OPEN {: archive:ZIP:archive :}
   archive CHECK-ZIP archive ZIP:CLOSE
   path bytes ZIP:OPEN-FILE {: file:ZIP:archive :}
   file CHECK-FILE file ZIP:CLOSE
   T-REPORT ;

;package
