\ A checked tools/wasm-build program for the task DATA and static image boundary.
require src/lib/string.f
require src/lib/fmt.f
require src/lib/ieee754.f

package WASM-DATA-REGION
private

DYNAMIC-BUFFER BYTES u8

public

: MAIN ( -- )
   11 22 depth 2 = if s" stack depth ok" type cr then 2drop
   SB-RESET $7FEFFFFFFFFFFFFF IEEE754:BITS>F64 16 FMT:SB-SCI
   SB$ type cr
   s" static owner intact" type cr
   204800 BYTES-RESERVE
   137 204799 BYTES c!
   204799 BYTES c@ .
   [: 204800 BYTES c@ drop ;] catch E-LAYOUT-BOUNDS = if
      s" bounds ok" type cr
   then
   0 BYTES BYTE-VIEW NULL-PTR BYTE-VIEW - . ;

;package
