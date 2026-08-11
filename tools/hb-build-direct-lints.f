\ hb-build-direct-lints.f - in-process lint hooks for hb-build gate callers.
\ Load after lint cores and tools/hb-build-lib.f.
\
\ The two hook installs name their target as HB-BUILD-CLI:HOOK rather than
\ bare: `is` resolves the name it parses through the engine's own lookup, which
\ does not consult the packages a `using` imported, so a bare target under an
\ open import fails to resolve (rc 70).

package HB-BUILD-DIRECT-LINTS
using HB-BUILD-CLI
private

: LINT-EXIT ( n -- ) {: rc:n :}
   rc 0= if exit then
   rc HBB-EXIT ;

: SIGNATURE-LINT ( -- )
   SIGNATURE-LINT:RESET
   HBB-JSON @ SIGNATURE-LINT:JSON!
   2 >FD SIGNATURE-LINT:OUT-FD!
   HBB-SRC$ SIGNATURE-LINT:FILE
   SIGNATURE-LINT:FINISH ;

: AOT-LINT ( -- )
   AOT-LINT:RESET
   HBB-JSON @ AOT-LINT:JSON!
   2 >FD AOT-LINT:OUT-FD!
   HBB-SRC$ AOT-LINT:FILE
   AOT-LINT:FINISH ;

: RUN-AOT ( -- )
   [: AOT-LINT ;] catch LINT-EXIT ;

: RUN-SIGNATURE ( -- )
   [: SIGNATURE-LINT ;] catch LINT-EXIT ;

: INSTALL ( -- )
   [: RUN-AOT ;] is HB-BUILD-CLI:HBB-AOT-LINT-HOOK
   [: RUN-SIGNATURE ;] is HB-BUILD-CLI:HBB-SIGNATURE-LINT-HOOK ;

INSTALL

;using
;package
