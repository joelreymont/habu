\ gate-diagnostics-all-strict-lib.f - SARIF-backed diagnostic slice.
\
\ Load after test/gate-diagnostics-lib.f.

require tools/diag-to-sarif-core.f

: GDX-SARIF ( -- )
   GE-HB-RESET
   s" habu-all-errors.err" GDX-PATH!
   [: GDX-PATH$ SARIF-FILE ;] GE-CAPTURE-ACTION GE-EVAL-STORE-RC
   s" diag-to-sarif" GE-EXPECT-OK
   s" habu-all-errors.sarif" GDX-WRITE-OUT
   s" sarif" s" habu-all-errors.sarif" s" sarif output" GDX-GJA1 ;

: GDX-ALL-STRICT-SLICE ( -- )
   s" hb-gate-diagnostics-all-strict" GT-START
   GDX-ALL-ERRORS
   GDX-SARIF
   GDX-STRICT-SIGNATURES
   GDX-BARE-PTR-SIGNATURE
   GDX-BAD-NOMINAL-DECL
   GDX-SOURCE-LOCAL-NOMINAL
   GDX-LOAD-FAIL-CLOSED
   GT-CLEANUP
   s" PASS: native checker diagnostics all-strict slice" type cr ;
