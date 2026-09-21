\ chain-plan-test.f - the engine-impact boundary used by chain runners.

require lib/test.f
require tools/chain-plan.f

package CHAIN-PLAN-TEST

: T-ENGINE ( ptr u8 n -- )
   CHAIN-PLAN:ENGINE? TTRUE ;

: T-FOCUSED ( ptr u8 n -- )
   CHAIN-PLAN:ENGINE? 0= TTRUE ;

: MAIN ( -- )
   T-RESET
   s" src/core/checker.f" T-ENGINE
   s" src/compiler/native/compiler.f" T-ENGINE
   s" bootstrap/cg/forth.fs" T-ENGINE
   s" tools/native-build-core.f" T-ENGINE
   s" lib/string.f" T-ENGINE
   s" lib/json-read.f" T-FOCUSED
   s" lib/net/curl.f" T-FOCUSED
   s" test/gate-stdlib-cases.f" T-FOCUSED
   s" docs/bootstrap.md" T-FOCUSED
   s" tools/chain-plan.f" T-FOCUSED
   T-REPORT ;

MAIN
;package
