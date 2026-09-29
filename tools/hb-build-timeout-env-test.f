\ hb-build-timeout-env-test.f - checked maker deadline override validation.
\ Run: bin/hb --load tools/hb-build-timeout-env-test.f

require tools/hb-build-test-lib.f

package HB-BUILD-CLI

: HBT-INVALID-TIMEOUT ( ptr u8 n -- )
   HBT-ARGV-BASE
   HBT-TIMEOUT-ENV
   HBT-ADD-TIMEOUT-ARGS
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc HBB-USAGE-RC T=
   outu 0 T=
   HBT-ERR erru s" HB_BUILD_TIMEOUT_MS" CONTAINS? TTRUE
   HBT-REPL-BAD-OUT EXISTS? TFALSE ;

: HBT-TIMEOUT-ENV-CASES ( -- )
   s" 0" HBT-INVALID-TIMEOUT
   s" -1" HBT-INVALID-TIMEOUT
   s" nope" HBT-INVALID-TIMEOUT ;

public
: HBT-TIMEOUT-ENV-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-TIMEOUT-ENV-CASES
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-timeout-env-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-TIMEOUT-ENV-MAIN
