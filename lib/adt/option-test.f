\ option-test.f - constructor payload and nominal-role refusals.

require lib/test.f
require test/checker-assert.f
require lib/adt/option.f

T-RESET

package OPTION-TEST

private

: YES ( ptr u8 n -- ) CHECK-QUIET-CANDIDATE! -1 T= ;
: NO ( ptr u8 n -- ) CHECK-QUIET-CANDIDATE! 0 T= ;
: EFFECTS ( -- )
   s" OPT-NONE-N ( -- option<n> ) OPTION:NONE" YES
   s" OPT-NONE-R ( -- option<r> ) OPTION:NONE" YES
   s" OPT-SOME-N ( n -- option<n> ) OPTION:SOME" YES
   s" OPT-SOME-I ( idx -- option<idx> ) OPTION:SOME" YES
   s" OPT-X-NONE ( n -- option<n> ) OPTION:NONE" NO
   s" OPT-X-SOME ( -- option<n> ) OPTION:SOME" NO
   s" OPT-X-ROLE ( len -- option<idx> ) OPTION:SOME" NO
   s" OPT-X-INST ( idx -- option<len> ) OPTION:SOME" NO ;

public

: RUN ( -- )
   s" constructor effects and nominal roles" T-LABEL EFFECTS
   T-REPORT ;

;package

OPTION-TEST:RUN

