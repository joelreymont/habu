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

: GDX-ARG-FLAG? ( ptr u8 n -- bool )
   s" --update-golden" STR= ;

\ Slice selection ignores the golden flag so `-- <slice> --update-golden`
\ regenerates goldens for that slice.
: GDX-EFFECTIVE-ARGC ( -- n )
   0
   0 begin dup SCRIPT-ARGC < while
      dup SCRIPT-ARGV$ GDX-ARG-FLAG? 0= if swap 1+ swap then
      1+
   repeat drop ;

: GDX-SLICE$ ( -- ptr u8 n )
   0 begin dup SCRIPT-ARGC < while
      dup SCRIPT-ARGV$ GDX-ARG-FLAG? 0= if SCRIPT-ARGV$ exit then
      1+
   repeat drop s" " ;

: GDX-DISPATCH ( -- )
   GOLD:INIT
   GDX-EFFECTIVE-ARGC 0= if GDX-SERIAL exit then
   GDX-EFFECTIVE-ARGC 1 <> if GDX-USAGE then
   GDX-SLICE$ s" diag-repair" STR= if GDX-REPAIR-SLICE exit then
   GDX-SLICE$ s" diag-undef-primary" STR= if GDX-UNDEF-PRIMARY-SLICE exit then
   GDX-SLICE$ s" diag-all-strict" STR= if GDX-ALL-STRICT-SLICE exit then
   GDX-SLICE$ s" diag-file-unsafe" STR= if GDX-FILE-UNSAFE-SLICE exit then
   GDX-USAGE ;
