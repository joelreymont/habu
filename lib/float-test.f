\ float-test.f - checked STR>FLOAT coverage.
\ Run: bin/hb --load lib/float-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/float.f
require lib/ieee754.f
require lib/test/guard-page.f
require src/core/bytes.f

: FL-NEAR ( r r -- bool ) f- fabs 0.000001 f< ;

: T-FL ( ptr u8 n r -- ) {: want:r :}               \ parse string, expect want
   STR>FLOAT MATCH option
     none OF 0 0= 0= ENDOF                           \ none -> false (unexpected)
     some OF want FL-NEAR ENDOF                       \ some(r) -> r ~ want
   ;MATCH T-ASSERT ;
: T-FL-BAD ( ptr u8 n -- )                          \ parse string, expect NONE
   STR>FLOAT MATCH option
     none OF 0 0= ENDOF                              \ none -> true
     some OF drop 0 0= 0= ENDOF                       \ some -> false (unexpected)
   ;MATCH T-ASSERT ;
: T-FL-OK ( ptr u8 n -- )                           \ parse string, expect SOME
   STR>FLOAT MATCH option
     none OF false ENDOF
     some OF drop true ENDOF
   ;MATCH T-ASSERT ;

: FL-RUN ( -- )
   T-RESET
   s" 3.14"    3.14    T-FL
   s" -0.5"    -0.5    T-FL
   s" 45.0"    45.0    T-FL
   s" 52.5"    52.5    T-FL
   s" 100"     100.0   T-FL
   s" 0"       0.0     T-FL
   s" .5"      0.5     T-FL
   s" 5."      5.0     T-FL
   s" 1e3"     1000.0  T-FL
   s" 1.5e2"   150.0   T-FL
   s" -2.5E-3" -0.0025 T-FL
   s" +7"      7.0     T-FL
   s" 6285"    6285.0  T-FL
   s" 0.000001" 0.000001 T-FL
   s" -0.008503115738340623" -0.008503115738340623 T-FL
   s" 9223372036854775807.0" T-FL-OK
   s" -9223372036854775807.0" T-FL-OK
   s" "       T-FL-BAD
   s" ."      T-FL-BAD
   s" abc"    T-FL-BAD
   s" 1.2.3"  T-FL-BAD
   s" 1e"     T-FL-BAD
   s" -"      T-FL-BAD
   s" 1.2e3x" T-FL-BAD
   s" -0.0085031157383406233" T-FL-BAD
   s" 0.0000000000000000000" T-FL-BAD
   s" 9223372036854775808.0" T-FL-BAD
   s" -9223372036854775808.0" T-FL-BAD ;

package DECIMAL-FLOAT-TEST

\ Expected encodings are independently rounded IEEE binary64 decimal values.
\ Normal results allow 16 ULPs for the bounded sequence of rounded operations;
\ this tolerance may never admit zero, infinity, NaN, or the wrong sign.
: WITHIN-ULPS? ( n n n -- bool ) {: got:n want:n ulps:n :}
   got 63 rshift want 63 rshift <> if false exit then
   got $7FFFFFFFFFFFFFFF and {: magnitude:n :}
   magnitude $7FF0000000000000 >= if false exit then
   magnitude 0= if false exit then
   got want - abs ulps <= ;


: CHECK-BITS ( ptr u8 n n n -- ) {: a:ptr u:n want:n ulps:n :}
   a u 80 min T-LABEL
   a u STR>FLOAT MATCH option
     none OF false ENDOF
     some OF IEEE754:F64>BITS {: got:n :}
       ulps 0= if got want = else got want ulps WITHIN-ULPS? then
     ENDOF
   ;MATCH TTRUE ;


: ZERO-AND-RANGE ( -- )
   s" POW10+ keeps negative exponents bounded" T-LABEL
   -1 POW10+ IEEE754:F64>BITS $3FF0000000000000 T=
   0 POW10+ IEEE754:F64>BITS $3FF0000000000000 T=
   s" 0e400" $0 0 CHECK-BITS
   s" -0e400" $8000000000000000 0 CHECK-BITS
   s" +0e-400" $0 0 CHECK-BITS
   s" -0e9223372036854775807" $8000000000000000 0 CHECK-BITS
   s" 1e-309" $0000B8157268FDAF 0 CHECK-BITS
   s" -1e-309" $8000B8157268FDAF 0 CHECK-BITS
   s" 1000e-309" $0066789E3750F791 16 CHECK-BITS
   s" -1000e-309" $8066789E3750F791 16 CHECK-BITS
   s" 1e308" $7FE1CCF385EBC8A0 16 CHECK-BITS
   s" 1.7976931348623157e308" $7FEFFFFFFFFFFFFF 16 CHECK-BITS
   s" 2.2250738585072014e-308" $0010000000000000 16 CHECK-BITS
   s" 4.9406564584124654e-324" $1 0 CHECK-BITS
   s" 2.4e-324" $0 0 CHECK-BITS
   s" 2.5e-324" $1 0 CHECK-BITS
   s" -1e-400" $8000000000000000 0 CHECK-BITS
   s" 1e400" $7FF0000000000000 0 CHECK-BITS
   s" -1e400" $FFF0000000000000 0 CHECK-BITS ;


: EXPONENT-LIMITS ( -- )
   s" 10e9223372036854775807" $7FF0000000000000 0 CHECK-BITS
   s" 0.1e-9223372036854775808" $0 0 CHECK-BITS
   s" -1e-9223372036854775808" $8000000000000000 0 CHECK-BITS
   s" 1e9223372036854775808" T-FL-BAD
   s" 1e-9223372036854775809" T-FL-BAD
   s" 0e" T-FL-BAD
   s" 0e+" T-FL-BAD
   s" 0e-" T-FL-BAD
   s" 0e400x" T-FL-BAD
   s" 0ee400" T-FL-BAD
   s" 0e4E0" T-FL-BAD
   s" 0.0.0e400" T-FL-BAD
   s" 1e 400" T-FL-BAD ;


create LONG-TOKEN 512 allot

: ZERO-DIGITS ( -- )
   512 0 ?do $30 LONG-TOKEN i + c! loop ;


: LONG-INTEGER ( -- )
   ZERO-DIGITS
   $31 LONG-TOKEN c!
   s" e-400" LONG-TOKEN 401 + swap BYTE-COPY
   LONG-TOKEN 406 $3FF0000000000000 0 CHECK-BITS
   \ Huge positive exponent plus omitted integer digits cannot wrap negative.
   s" e9223372036854775807" LONG-TOKEN 401 + swap BYTE-COPY
   LONG-TOKEN 421 $7FF0000000000000 0 CHECK-BITS ;


: LONG-FRACTION ( -- )
   ZERO-DIGITS
   $2E LONG-TOKEN c!
   $31 LONG-TOKEN 400 + c!
   s" e400" LONG-TOKEN 401 + swap BYTE-COPY
   LONG-TOKEN 405 $3FF0000000000000 0 CHECK-BITS
   \ A long, nonzero fractional numerator must not become infinity / infinity.
   ZERO-DIGITS
   $2E LONG-TOKEN c!
   $31 LONG-TOKEN 1+ c!
   s" e1" LONG-TOKEN 401 + swap BYTE-COPY
   LONG-TOKEN 403 $3FF0000000000000 0 CHECK-BITS
   $78 LONG-TOKEN 390 + c!
   LONG-TOKEN 403 T-FL-BAD ;


: LONG-PRECISION ( -- )
   s" 123456789012345678901234567890e-29" $3FF3C0CA428C59FB 16 CHECK-BITS
   s" 000000000000000000000000001.25e0" $3FF4000000000000 0 CHECK-BITS
   s" 125000000000000000000000000e-26" $3FF4000000000000 0 CHECK-BITS ;


public

: RUN ( -- )
   ZERO-AND-RANGE
   EXPONENT-LIMITS
   LONG-INTEGER
   LONG-FRACTION
   LONG-PRECISION ;

;package

\ A negative length is refused before a byte is read; each span ends at an
\ inaccessible page, so a read faults. Unrefused, STR>FLOAT read the first byte,
\ answered SOME for a sign and -1, and for a sign and the minimum cell `u 1-`
\ wrapped to the maximum and the exponent scan ran past the span.
: FL-DROP ( option<r> -- )
   MATCH option
     none OF ENDOF
     some OF drop ENDOF
   ;MATCH ;

: FL-EDGE-NEG ( n -- ) {: u:n :}
   0 0 GUARD-PAGE:TAIL u STR>FLOAT FL-DROP ;

: FL-SIGNED-NEG ( n n -- ) {: sign:n u:n :}
   2 [char] 1 GUARD-PAGE:TAIL {: a:ptr :}
   sign a c!
   a u STR>FLOAT FL-DROP ;

: FL-NEGATIVE-LENGTHS ( -- )
   [: STR-MINUS -1 FL-SIGNED-NEG ;] E-STR-BOUNDS TTHROWSQ
   [: STR-PLUS -1 FL-SIGNED-NEG ;] E-STR-BOUNDS TTHROWSQ
   [: STR-MINUS STR-MIN-I64 FL-SIGNED-NEG ;] E-STR-BOUNDS TTHROWSQ
   [: STR-PLUS STR-MIN-I64 FL-SIGNED-NEG ;] E-STR-BOUNDS TTHROWSQ
   [: -1 FL-EDGE-NEG ;] E-STR-BOUNDS TTHROWSQ
   [: STR-MIN-I64 FL-EDGE-NEG ;] E-STR-BOUNDS TTHROWSQ ;

FL-RUN
DECIMAL-FLOAT-TEST:RUN
FL-NEGATIVE-LENGTHS
T-REPORT
