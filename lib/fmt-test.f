\ fmt-test.f - checked number formatting coverage.
\ Run: cat lib/errors.f lib/string.f lib/test.f lib/float.f lib/fmt.f lib/fmt-test.f | bin/hb

: T-U ( n ptr u8 n -- ) {: a:ptr u :}            \ SB-U n == a/u
   SB-RESET SB-U  SB$ a u T$= ;
: T-INT ( n ptr u8 n -- ) {: a:ptr u :}
   SB-RESET SB-INT  SB$ a u T$= ;
: T-FIX ( r n ptr u8 n -- ) {: a:ptr u :}        \ ( value places expect )
   SB-RESET SB-FIX  SB$ a u T$= ;

: FMT-RUN ( -- )
   T-RESET
   0     s" 0"      T-U
   7     s" 7"      T-U
   12345 s" 12345"  T-U
   -7    s" -7"     T-INT
   42    s" 42"     T-INT
   0     s" 0"      T-INT
   3.14    2 s" 3.14"   T-FIX
   0.5     3 s" 0.500"  T-FIX
   -2.5    1 s" -2.5"   T-FIX
   52.5    1 s" 52.5"   T-FIX
   100.0   1 s" 100.0"  T-FIX
   9.999   2 s" 10.00"  T-FIX         \ round half-up carries into the integer part
   45.0    0 s" 45"     T-FIX         \ zero places omits the point
   0.0     2 s" 0.00"   T-FIX ;

FMT-RUN
T-REPORT
