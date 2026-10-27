\ Native/Wasm numeric text fixture. Differential runner loads this source and
\ reaches STEP through its checked entry, including JSON's float accessor.
require src/lib/string.f
require src/lib/fmt.f
require src/lib/float.f
require src/lib/json-read.f
require src/lib/adt/option.f
require src/lib/ieee754.f

package WNUM
private

TYPED-VARIABLE READER-SPACE JR:cursor

: PARSED? ( ptr u8 n -- bool )
   STR>FLOAT MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

: SHOW ( bool -- )
   if -1 else 0 then . ;

\ Allow normalization's repeated binary64 scaling a relative error of 1e-12.
\ This checks the persisted finite value, rather than only the text's shape.
: ROUNDTRIP? ( r -- bool )
   {: want:r :}
   SB-RESET want 16 FMT:SB-SCI
   SB$ STR>FLOAT MATCH option
      none OF false ENDOF
      some OF want f- fabs want fabs f/ 0.000000000001 f< ENDOF
   ;MATCH ;

: BAD-INF ( -- )
   SB-RESET $7FF0000000000000 IEEE754:BITS>F64 16 FMT:SB-SCI ;

: JSON-NUMBER ( -- )
   READER-SPACE s" -2.5e-3" JR:INIT
   JR:NEXT drop
   JR:FLOAT {: value:r :}
   SB-RESET value 4 FMT:SB-SCI SB$ type cr
   JR:CLOSE ;

public

: STEP ( -- )
   SB-RESET 1.25 16 FMT:SB-SCI SB$ type cr
   SB-RESET -0.0 16 FMT:SB-SCI SB$ type cr
   SB-RESET 0.125 4 FMT:SB-SCI SB$ type cr
   JSON-NUMBER
   s" 1e308" PARSED? SHOW
   s" 1e-309" PARSED? SHOW
   s" 9223372036854775808.0" PARSED? SHOW
   s" -9223372036854775808.0" PARSED? SHOW
   s" 0.0000000000000000000" PARSED? SHOW
   s" 1e+" PARSED? SHOW
   $7FEFFFFFFFFFFFFF IEEE754:BITS>F64 ROUNDTRIP? SHOW
   $0000000000000001 IEEE754:BITS>F64 ROUNDTRIP? SHOW
   $001FFFFFFFFFFFFF IEEE754:BITS>F64 ROUNDTRIP? SHOW
   12345.6789 ROUNDTRIP? SHOW
   ['] BAD-INF catch E-FMT-DOMAIN = SHOW ;

;package
