\ checker-model-manifest.f - Run checker schema and behavior cases in their declaring package.

require lib/test.f
require test/compiler/checker-model-cases.f

package CHECKER-MODEL-MANIFEST-TEST
public

: RUN ( -- )
   T-RESET
   CHECKER-MODEL-CASES:HABU-SIDE
   T-REPORT ;

;package

\ Construct families resolve in their declaring package; the cases assert that
\ authority so a wrong scope cannot turn a negative case into a false pass.
package CHECKER-MODEL-CASES
CHECKER-MODEL-MANIFEST-TEST:RUN
;package
