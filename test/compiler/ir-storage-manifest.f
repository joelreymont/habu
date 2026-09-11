\ Native storage behavior for the shared example rows. Checks arena contents,
\ counts and named refusals, small scratch allocations, module budgets and depth.
\ This entry does not inspect source bodies or invoke Rocq.

require lib/test.f
require test/compiler/ir-storage-cases.f

package IR-STORAGE-MANIFEST-TEST
private

public

: RUN ( -- )
   T-RESET
   COMPILER-STORE-CASES:HABU-SIDE
   T-REPORT ;

;package

IR-STORAGE-MANIFEST-TEST:RUN
