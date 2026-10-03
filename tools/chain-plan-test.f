\ chain-plan-test.f - the engine-impact boundary used by chain runners.

require lib/test.f
require lib/fs.f
require lib/process.f
require lib/process-command.f
require lib/engine-candidate.f
require tools/chain-plan.f

package CHAIN-PLAN-TEST

: T-ENGINE ( ptr u8 n -- )
   CHAIN-PLAN:ENGINE? TTRUE ;

: T-FOCUSED ( ptr u8 n -- )
   CHAIN-PLAN:ENGINE? 0= TTRUE ;

\ The CLI entry on a revision jj cannot resolve: REV-PLAN throws, and the entry
\ names the code and exits the planner's failure status.
: T-REFUSED-REVISION ( -- )
   s" a refused revision exits the planner with FAIL-RC" T-LABEL
   PROC-CMD:RESET
   s" --load" >LEN PROC-CMD:ARG+
   s" tools/chain-plan-build.f" >LEN PROC-CMD:ARG+
   s" --" >LEN PROC-CMD:ARG+
   s" chain-plan-test-no-such-revision" >LEN PROC-CMD:ARG+
   SOURCE-ROOT:CWD$ >LEN PROC-CMD:ARG+
   ENGINE-CANDIDATE:PATH$ >LEN 60000 >MS PROC-CMD:RUN-OUTCOME
   PROC-OUTCOME>RC RC>N CHAIN-PLAN:FAIL-RC T=
   PROC-CMD:OUT$ s" chain-plan: uncaught throw code " CONTAINS? TTRUE ;

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
   T-REFUSED-REVISION
   T-REPORT ;

MAIN
;package
