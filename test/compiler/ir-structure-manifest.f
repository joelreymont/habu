\ ir-structure-manifest.f - Run native compiler structure schema, source and behavioral checks.

require lib/test.f
require test/compiler/ir-structure-cases.f

package IR-STRUCTURE-MANIFEST-TEST
public

: RUN ( -- )
   T-RESET
   COMPILER-STRUCT-CASES:HABU-SIDE
   T-REPORT ;

;package

IR-STRUCTURE-MANIFEST-TEST:RUN
