\ gate-diagnostics-entry-lib.f - one serial diagnostic test.

require test/gate-diagnostics-lib.f
require test/gate-diagnostics-all-strict-lib.f

package GATE-DIAGNOSTICS

: SERIAL ( -- )
   s" hb-gate-diagnostics" GT-START
   PRIMARY-JSON
   UNKNOWN-SIGNATURE
   BARE-PTR-SIGNATURE
   MALFORMED-QUOTATION-SIGNATURE
   BAD-PARAM-SIGNATURE
   BAD-NOMINAL-DECL
   SOURCE-LOCAL-NOMINAL
   REPAIR-CLASSES
   FILE-ORIGIN
   LABEL-COPY
   UNSAFE-CHECKS
   CAP-TRUSTED
   CAP-TRUSTED-FFI
   LOCAL-IN-LOOP
   RENDER-CAP-CLOSED
   ADT-FAMILY
   ADT-VARIANT
   ADT-PAYLOAD-POS
   SIG-ARITY
   TFAM-DECL
   LOAD-CLOSED
   ALL-ERRORS
   UNDEFINED-RECURSIVE
   SARIF
   PUBLIC-SIGNATURES
   GT-CLEANUP
   s" PASS: native checker diagnostics" type cr ;

public

: RUN ( -- )
   SCRIPT-ARGC 0 <> if
      s" usage: bin/hb --load test/gate-diagnostics.f" 64 die
   then
   GOLD:INIT
   SERIAL ;

;package
