\ data-region.f - a program for test/wasm/data-test.f: a stack depth, a real's
\ text and a dynamic buffer, built by `tools/hb-build.f -- --target wasm`, whose
\ entry is MAIN.
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
   0 BYTES BYTE-VIEW NULL-PTR BYTE-VIEW - . ;

;package

: MAIN ( -- ) WASM-DATA-REGION:MAIN ;
