\ gate-diagnostics-entry-lib.f - diagnostic slice CLI dispatch.
\
\ Load after test/gate-diagnostics-lib.f.

require test/gate-diagnostics-all-strict-lib.f

: GDX-SERIAL ( -- )
   s" hb-gate-diagnostics" GT-START
   GDX-PRIMARY-JSON
   GDX-UNKNOWN-SIGNATURE
   GDX-BARE-PTR-SIGNATURE
   GDX-MALFORMED-QUOTATION-SIGNATURE
   GDX-BAD-PARAM-SIGNATURE
   GDX-BAD-NOMINAL-DECL
   GDX-SOURCE-LOCAL-NOMINAL
   GDX-REPAIR-CLASSES
   GDX-FILE-ORIGIN
   GDX-STRICT-SIGNATURES
   GDX-UNSAFE-CHECKS
   GDX-LOCAL-IN-LOOP
   GDX-LOAD-FAIL-CLOSED
   GDX-ALL-ERRORS
   GDX-UNDEFINED-RECURSIVE
   GDX-SARIF
   GDX-PUBLIC-SIGNATURES
   GDX-TRUST-LINT-STALE
   GT-CLEANUP
   s" PASS: native checker diagnostics gate phase" type cr ;

: GDX-DISPATCH ( -- )
   SCRIPT-ARGC 0= if GDX-SERIAL exit then
   SCRIPT-ARGC 1 <> if GDX-USAGE then
   s" diag-repair" GDX-ARG0= if GDX-REPAIR-SLICE exit then
   s" diag-undef-primary" GDX-ARG0= if GDX-UNDEF-PRIMARY-SLICE exit then
   s" diag-all-strict" GDX-ARG0= if GDX-ALL-STRICT-SLICE exit then
   s" diag-file-unsafe" GDX-ARG0= if GDX-FILE-UNSAFE-SLICE exit then
   GDX-USAGE ;
