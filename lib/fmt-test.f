\ fmt-test.f - checked number formatting coverage.
\ Run: cat lib/errors.f lib/string.f lib/test.f lib/float.f lib/fmt.f lib/fmt-test.f | bin/hb

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/float.f
require lib/fmt.f

\ White-box test: reopen the module's package so the fixtures call FMT's public
\ builders (SB-U / SB-INT / SB-FIX) by their bare package-local names.
package FMT

: T-U ( n ptr u8 n -- ) {: a:ptr u :}            \ SB-U n == a/u
   SB-RESET SB-U  SB$ a u T$= ;
: T-INT ( n ptr u8 n -- ) {: a:ptr u :}
   SB-RESET SB-INT  SB$ a u T$= ;
: T-FIX ( r n ptr u8 n -- ) {: a:ptr u :}        \ ( value places expect )
   SB-RESET SB-FIX  SB$ a u T$= ;

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

: FMT-RUN ( -- )
   T-RESET
   0     s" 0"      T-U
   7     s" 7"      T-U
   12345 s" 12345"  T-U
   99    s" 99"     T-U
   STR-MAX-I64 s" 9223372036854775807" T-U    \ largest unsigned i64 renders exactly
   -7    s" -7"     T-INT
   42    s" 42"     T-INT
   -123  s" -123"   T-INT
   0     s" 0"      T-INT
   STR-MAX-I64 s" 9223372036854775807" T-INT  \ i64 max
   STR-MIN-I64 s" -9223372036854775808" T-INT \ i64 min: no positive magnitude, table-emitted
   STR-MIN-I64 1+ s" -9223372036854775807" T-INT \ its neighbour, which negates without overflowing
   3.14    2 s" 3.14"   T-FIX
   0.5     3 s" 0.500"  T-FIX
   -2.5    1 s" -2.5"   T-FIX
   52.5    1 s" 52.5"   T-FIX
   100.0   1 s" 100.0"  T-FIX
   9.999   2 s" 10.00"  T-FIX         \ round half-up carries into the integer part
   45.0    0 s" 45"     T-FIX         \ zero places omits the point
   0.0     2 s" 0.00"   T-FIX
   9223372036854774784.0 0 s" 9223372036854774784" T-FIX  \ 2^63-1024: largest scaled magnitude that fits
   \ public printers: the shared builder survives a print
   99                   T-PU
   -123                 T-PINT
   STR-MIN-I64          T-PINT             \ the table-emitted magnitude prints too
   2.5     1 s" 2.5"    T-PFN
   \ fail-closed domain guards
   [: NEG-U-CASE ;]        E-FMT-DOMAIN   TTHROWSQ  \ negative into unsigned -> E-FMT-DOMAIN
   [: OVERFLOW-FIX-CASE ;]  E-FMT-OVERFLOW TTHROWSQ \ scaled overflow -> E-FMT-OVERFLOW
   [: OVERFLOW-2P63-CASE ;] E-FMT-OVERFLOW TTHROWSQ \ exactly 2^63 does not fit i64
   ;

FMT-RUN
T-REPORT

;package
