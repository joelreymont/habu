\ ffi-f32-native.f - C peer for the FUNCTION: binary32 ABI.
\ macOS: cc -dynamiclib -o build/ffi-f32-peer.dylib test/ffi-f32-peer.c
\ Linux: cc -shared -fPIC -o build/ffi-f32-peer.so test/ffi-f32-peer.c
\ Then: bin/hb --load test/ffi-f32-native.f

require src/lib/test.f
require src/lib/ffi-abi.f
require src/lib/float32.f
require src/lib/ieee754.f

package FFI-F32-TEST
private

: SELECT-PEER ( -- )
   HB-TARGET-MACOS? if s" build/ffi-f32-peer.dylib"
   else s" build/ffi-f32-peer.so" then FFI-DECL:SELECT-LIBRARY ;

SELECT-PEER

FUNCTION: MIX habu_f32_mix ( f32 r n f32 -- f32 ) ;FUNCTION
FUNCTION: REGISTERS habu_f32_registers
   ( n f32 n f32 n f32 n f32 n f32 n f32 n f32 n f32 -- r ) ;FUNCTION
FUNCTION: VARIADIC-FIXED habu_f32_variadic ( f32 n r -- f32 )
   2 VARIADIC
;FUNCTION

: RUN ( -- )
   T-RESET
   16777217.0 3.125 7 -0.0 MIX IEEE754:F64>BITS
      $3FB99999A0000000 T=
   10 0.5 11 1.5 12 2.5 13 3.5
   14 4.5 15 5.5 16 6.5 17 7.5 REGISTERS
      1234.5 f= T-ASSERT
   1.25 1 2.5 VARIADIC-FIXED 3.75 f= T-ASSERT
   T-REPORT ;

RUN

;package
