\ gate-diagnostics-entry-lib.f - diagnostic slice CLI dispatch.
\
\ Load after test/gate-diagnostics-lib.f.

require test/gate-diagnostics-all-strict-lib.f

package GATE-DIAGNOSTICS

using GATE

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
   STRICT-SIGNATURES
   UNSAFE-CHECKS
   LOCAL-IN-LOOP
   LOAD-CLOSED
   ALL-ERRORS
   UNDEFINED-RECURSIVE
   SARIF
   PUBLIC-SIGNATURES
   LINT-STALE
   GT-CLEANUP
   s" PASS: native checker diagnostics gate phase" type cr ;

: ARG-FLAG? ( ptr u8 n -- bool )
   s" --update-golden" STR= ;

\ Slice selection ignores the golden flag so `-- <slice> --update-golden`
\ regenerates goldens for that slice.
: EFFECTIVE-ARGC ( -- n )
   0
   0 begin dup SCRIPT-ARGC < while
      dup SCRIPT-ARGV$ ARG-FLAG? 0= if swap 1+ swap then
      1+
   repeat drop ;

: SLICE$ ( -- ptr u8 n )
   0 begin dup SCRIPT-ARGC < while
      dup SCRIPT-ARGV$ ARG-FLAG? 0= if SCRIPT-ARGV$ exit then
      1+
   repeat drop s" " ;

: DISPATCH ( -- )
   GOLD:INIT
   EFFECTIVE-ARGC 0= if SERIAL exit then
   EFFECTIVE-ARGC 1 <> if USAGE then
   SLICE$ s" diag-repair" STR= if REPAIR exit then
   SLICE$ s" diag-undef-primary" STR= if UNDEFINED-PRIMARY exit then
   SLICE$ s" diag-all-strict" STR= if ALL-STRICT exit then
   SLICE$ s" diag-file-unsafe" STR= if FILE-UNSAFE exit then
   SLICE$ s" diag-label-copy" STR= if LABEL-COPY-SLICE exit then
   USAGE ;

;package
