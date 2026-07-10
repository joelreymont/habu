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

\ --- switchover wave A: FL-FIND-E now returns option<idx> (SOME index of e/E,
\ else NONE) and FL-PARSE-EXP MATCHes it. Test both words directly.
: T-FE ( ptr u8 n n -- ) {: want:n :}               \ FL-FIND-E: some idx -> want, none -> -1
   FL-FIND-E MATCH option
     none OF -1 ENDOF
     some OF IDX>N ENDOF
   ;MATCH  want T= ;
: FL-RUN-EXP ( -- )
   \ FL-FIND-E: found -> SOME(index of e/E); absent -> NONE.
   s" 1e3"   1 T-FE
   s" 1E3"   1 T-FE
   s" 1.5e2" 3 T-FE
   s" 123"  -1 T-FE
   s" 3.14" -1 T-FE
   \ FL-PARSE-EXP: the some branch returns the mantissa length (before the e);
   \ the none branch returns the whole length. Both drive the STR>FLOAT cases above.
   s" 1e3"  FL-PARSE-EXP 1 T=
   s" 12e5" FL-PARSE-EXP 2 T=
   s" 123"  FL-PARSE-EXP 3 T= ;

\ --- switchover wave A: FL-SIG now returns option<r> (SOME significand, else
\ NONE). It is STR>FLOAT's mantissa parser; test its both branches directly.
: T-FS ( ptr u8 n r -- ) {: want:r :}               \ FL-SIG: some(r) near want, none -> fail
   FL-SIG MATCH option
     none OF 0 0= 0= ENDOF                           \ none -> false (unexpected)
     some OF want FL-NEAR ENDOF                       \ some(r) -> r ~ want
   ;MATCH T-ASSERT ;
: T-FS-BAD ( ptr u8 n -- )                          \ FL-SIG: expect NONE
   FL-SIG MATCH option
     none OF 0 0= ENDOF                              \ none -> true
     some OF drop 0 0= 0= ENDOF                       \ some -> false (unexpected)
   ;MATCH T-ASSERT ;
: FL-RUN-SIG ( -- )
   \ FL-SIG: valid unsigned mantissa -> SOME(value); bad/no-digit -> NONE.
   s" 3.14"  3.14  T-FS
   s" 100"   100.0 T-FS
   s" .5"    0.5   T-FS
   s" 5."    5.0   T-FS
   s" 0"     0.0   T-FS
   s" "      T-FS-BAD
   s" ."     T-FS-BAD
   s" abc"   T-FS-BAD
   s" 1.2.3" T-FS-BAD ;

FL-RUN
FL-RUN-EXP
FL-RUN-SIG
T-REPORT
