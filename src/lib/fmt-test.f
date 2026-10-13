\ fmt-test.f - checked number formatting coverage.
\ Run: bin/hb --load src/lib/fmt-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/float.f
require src/lib/fmt.f
require src/lib/ieee754.f

\ White-box test: reopen the module's package so the fixtures call FMT's public
\ builders (SB-U / SB-HEX / SB-INT / SB-FIX) by their bare package-local names.
package FMT

: T-U ( n ptr u8 n -- ) {: a:ptr u :}            \ SB-U n == a/u
   SB-RESET SB-U  SB$ a u T$= ;
: T-HEX ( n ptr u8 n -- ) {: a:ptr u :}
   SB-RESET s" pre:" SB-APPEND SB-HEX  SB$ a u T$= ;
: T-INT ( n ptr u8 n -- ) {: a:ptr u :}
   SB-RESET SB-INT  SB$ a u T$= ;
: T-SCALED ( n n ptr u8 n -- ) {: a:ptr u :}
   SB-RESET s" pre:" SB-APPEND SB-SCALED  SB$ a u T$= ;
: T-FIX ( r n ptr u8 n -- ) {: a:ptr u :}        \ ( value places expect )
   SB-RESET SB-FIX  SB$ a u T$= ;
: T-SCI ( r n ptr u8 n -- ) {: a:ptr u :}
   SB-RESET SB-SCI SB$ a u T$= ;

\ .U and .INT render the same bytes SB-U / SB-INT append -- one renderer, one
\ set of digit cases above -- and type them without disturbing the builder. The
\ sentinel is deliberately not the printed number: a printer that reset SB
\ would leave that number's digits in the buffer instead.
: T-PU ( n -- )
   SB-RESET s" sb-sentinel" SB-APPEND
   .U    SB$ s" sb-sentinel" T$= ;
: T-PINT ( n -- )
   SB-RESET s" sb-sentinel" SB-APPEND
   .INT  SB$ s" sb-sentinel" T$= ;
\ F.N has no bounded width and does build in the shared builder, so its text is
\ still readable through SB$ after the call.
: T-PFN ( r n ptr u8 n -- ) {: a:ptr u:n :}      \ ( value places expect )
   F.N   SB$ a u T$= ;

\ Fail-closed negative cases (quotation bodies for TTHROWSQ, each ( -- )).
: NEG-U-CASE ( -- )        SB-RESET -1 SB-U ;                            \ unsigned rejects negative
: OVERFLOW-FIX-CASE ( -- ) SB-RESET 1000000000000000000.0 2 SB-FIX ;     \ 1e18 * 10^2 overflows i64
: OVERFLOW-2P63-CASE ( -- ) SB-RESET 4611686018427387904.0 2.0 f* 0 SB-FIX ;  \ boundary is exclusive at 2^63
: SCALED-LOW-CASE ( -- ) 1 -1 SB-SCALED ;
: SCALED-HIGH-CASE ( -- ) 1 19 SB-SCALED ;
: SCI-LOW-CASE ( -- ) 1.0 -1 SB-SCI ;
: SCI-HIGH-CASE ( -- ) 1.0 18 SB-SCI ;
: SCI-INF-CASE ( -- ) $7FF0000000000000 IEEE754:BITS>F64 16 SB-SCI ;
: SCI-NAN-CASE ( -- ) $7FF8000000000000 IEEE754:BITS>F64 16 SB-SCI ;

: FMT-RUN ( -- )
   T-RESET
   0     s" 0"      T-U
   7     s" 7"      T-U
   12345 s" 12345"  T-U
   99    s" 99"     T-U
   STR-MAX-I64 s" 9223372036854775807" T-U    \ largest unsigned i64 renders exactly
   0     s" pre:0" T-HEX
   9     s" pre:9" T-HEX
   10    s" pre:A" T-HEX
   15    s" pre:F" T-HEX
   16    s" pre:10" T-HEX
   $7FFF s" pre:7FFF" T-HEX
   STR-MAX-I64 s" pre:7FFFFFFFFFFFFFFF" T-HEX
   -7    s" -7"     T-INT
   42    s" 42"     T-INT
   -123  s" -123"   T-INT
   0     s" 0"      T-INT
   STR-MAX-I64 s" 9223372036854775807" T-INT  \ i64 max
   STR-MIN-I64 s" -9223372036854775808" T-INT \ i64 min: no positive magnitude, table-emitted
   STR-MIN-I64 1+ s" -9223372036854775807" T-INT \ its neighbour, which negates without overflowing
   5500 3 s" pre:5.5" T-SCALED
   5001 3 s" pre:5.001" T-SCALED
   -1 3 s" pre:-0.001" T-SCALED
   5000 3 s" pre:5" T-SCALED
   -5000 3 s" pre:-5" T-SCALED
   0 18 s" pre:0" T-SCALED
   1 18 s" pre:0.000000000000000001" T-SCALED
   -1 18 s" pre:-0.000000000000000001" T-SCALED
   10 18 s" pre:0.00000000000000001" T-SCALED
   STR-MAX-I64 18 s" pre:9.223372036854775807" T-SCALED
   STR-MIN-I64 18 s" pre:-9.223372036854775808" T-SCALED
   STR-MIN-I64 0 s" pre:-9223372036854775808" T-SCALED
   1000000000000000000 18 s" pre:1" T-SCALED
   3.14    2 s" 3.14"   T-FIX
   0.5     3 s" 0.500"  T-FIX
   -2.5    1 s" -2.5"   T-FIX
   52.5    1 s" 52.5"   T-FIX
   100.0   1 s" 100.0"  T-FIX
   9.999   2 s" 10.00"  T-FIX         \ round half-up carries into the integer part
   45.0    0 s" 45"     T-FIX         \ zero places omits the point
   0.0     2 s" 0.00"   T-FIX
   9223372036854774784.0 0 s" 9223372036854774784" T-FIX  \ 2^63-1024: largest scaled magnitude that fits
   0.0 16 s" 0.0000000000000000e0" T-SCI
   -0.0 16 s" -0.0000000000000000e0" T-SCI
   1.5 2 s" 1.50e0" T-SCI
   -150.0 2 s" -1.50e2" T-SCI
   0.00125 4 s" 1.2500e-3" T-SCI
   1.25 1 s" 1.3e0" T-SCI              \ exact halfway rounds away from zero
   -1.25 1 s" -1.3e0" T-SCI
   9.5 0 s" 1e1" T-SCI                 \ rounding carry changes exponent
   $0000000000000001 IEEE754:BITS>F64 16
      s" 4.9406564584124654e-324" T-SCI
   $0010000000000000 IEEE754:BITS>F64 16
      s" 2.2250738585072014e-308" T-SCI
   $7FEFFFFFFFFFFFFF IEEE754:BITS>F64 16
      s" 1.7976931348623157e308" T-SCI
   $7FEFFFFFFFFFFFFF IEEE754:BITS>F64 17
      s" 1.79769313486231571e308" T-SCI
   $7FEFFFFFFFFFFFFF IEEE754:BITS>F64 0
      s" 2e308" T-SCI                 \ coarse decimal rounding may exceed binary64 range
   \ public printers: the shared builder survives a print
   99                   T-PU
   -123                 T-PINT
   STR-MIN-I64          T-PINT             \ the table-emitted magnitude prints too
   2.5     1 s" 2.5"    T-PFN
   \ fail-closed domain guards
   [: NEG-U-CASE ;]        E-FMT-DOMAIN   TTHROWSQ  \ negative into unsigned -> E-FMT-DOMAIN
   SB-RESET s" carry" SB-APPEND
   [: -1 SB-HEX ;] E-FMT-DOMAIN TTHROWSQ
   SB$ s" carry" T$=
   [: OVERFLOW-FIX-CASE ;]  E-FMT-OVERFLOW TTHROWSQ \ scaled overflow -> E-FMT-OVERFLOW
   [: OVERFLOW-2P63-CASE ;] E-FMT-OVERFLOW TTHROWSQ \ exactly 2^63 does not fit i64
   [: SCI-LOW-CASE ;] E-FMT-DOMAIN TTHROWSQ
   [: SCI-HIGH-CASE ;] E-FMT-DOMAIN TTHROWSQ
   [: SCI-INF-CASE ;] E-FMT-DOMAIN TTHROWSQ
   [: SCI-NAN-CASE ;] E-FMT-DOMAIN TTHROWSQ
   SB-RESET s" carry" SB-APPEND
   [: SCALED-LOW-CASE ;] E-FMT-DOMAIN TTHROWSQ
   SB$ s" carry" T$=
   [: SCALED-HIGH-CASE ;] E-FMT-DOMAIN TTHROWSQ
   SB$ s" carry" T$=
   ;

FMT-RUN
T-REPORT

;package
