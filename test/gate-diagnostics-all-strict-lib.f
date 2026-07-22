\ gate-diagnostics-all-strict-lib.f - SARIF-backed diagnostic slice.
\
\ Load after test/gate-diagnostics-lib.f.

require tools/diag-to-sarif-core.f

package GATE-DIAGNOSTICS

: SARIF ( -- )
   GE-HB-RESET
   s" habu-all-errors.err" PATH!
   [: PATH$ SARIF-FILE ;] GE-CAPTURE-ACTION GE-EVAL-STORE-RC
   s" diag-to-sarif" GE-EXPECT-OK
   s" habu-all-errors.sarif" WRITE-OUT
   s" diag-all-errors.sarif" s" sarif golden" OUT-GOLDEN-R
   s" sarif" s" habu-all-errors.sarif" s" sarif output" GJA1 ;

public

: ALL-STRICT ( -- )
   s" hb-gate-diagnostics-all-strict" GT-START
   ALL-ERRORS
   SARIF
   STRICT-SIGNATURES
   BARE-PTR-SIGNATURE
   BAD-NOMINAL-DECL
   SOURCE-LOCAL-NOMINAL
   LOAD-CLOSED
   GT-CLEANUP
   s" PASS: native checker diagnostics all-strict slice" type cr ;

;package
