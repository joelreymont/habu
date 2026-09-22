\ gate-diagnostics-all-strict-lib.f - SARIF assertion for diagnostics.

require tools/diag-to-sarif-core.f
require test/gate-diagnostics-lib.f

package GATE-DIAGNOSTICS

: SARIF ( -- )
   GE-HB-RESET
   s" habu-all-errors.err" PATH!
   [: PATH$ SARIF-FILE ;] GE-CAPTURE-ACTION GE-EVAL-STORE-RC
   s" diag-to-sarif" GE-EXPECT-OK
   s" habu-all-errors.sarif" WRITE-OUT
   s" diag-all-errors.sarif" s" sarif golden" OUT-GOLDEN-R
   s" sarif" s" habu-all-errors.sarif" s" sarif output" GJA1 ;

;package
