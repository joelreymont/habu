\ ir-storage-manifest.f - Run native storage and lifetime cases against the compiler runtime.

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
