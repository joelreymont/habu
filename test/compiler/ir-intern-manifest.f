\ ir-intern-manifest.f - Run native interning schema, source and behavioral checks.

require lib/test.f
require test/compiler/ir-intern-cases.f

package IR-INTERN-MANIFEST-TEST
private

public

: RUN ( -- )
   T-RESET
   COMPILER-INTERN-CASES:HABU-SIDE
   T-REPORT ;

;package

IR-INTERN-MANIFEST-TEST:RUN
