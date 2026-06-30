\ float-test.f - checked STR>FLOAT coverage.
\ Run: cat lib/errors.f lib/string.f lib/test.f lib/float.f lib/float-test.f | bin/hb

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/float.f

: FL-NEAR ( r r -- bool ) f- fabs 0.000001 f< ;

: T-FL ( ptr u8 n r -- ) {: want :}                 \ parse string, expect want
   STR>FLOAT {: ok :}
   want FL-NEAR ok and T-ASSERT ;
: T-FL-BAD ( ptr u8 n -- )                          \ parse string, expect failure
   STR>FLOAT {: ok :} drop ok 0= T-ASSERT ;

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
   s" "       T-FL-BAD
   s" ."      T-FL-BAD
   s" abc"    T-FL-BAD
   s" 1.2.3"  T-FL-BAD
   s" 1e"     T-FL-BAD
   s" -"      T-FL-BAD
   s" 1.2e3x" T-FL-BAD ;

FL-RUN
T-REPORT
