\ chain-plan-build.f - CLI entry for the checked chain planner.
\
\ Exit status: 0 with the plan on stdout; 64 for a refused command line;
\ PROC-TIMEOUT-RC (124, lib/process.f) when a deadline expired;
\ CHAIN-PLAN:FAIL-RC for every other throw, which it names first as
\ `chain-plan: uncaught throw code N`.

require lib/process.f
require tools/chain-plan.f

package CHAIN-PLAN-CLI

: PLAN ( -- )
   SCRIPT-ARGC 2 <> if s" chain-plan: revision and repository root are required" 64 die then
   0 SCRIPT-ARGV$ 1 SCRIPT-ARGV$ CHAIN-PLAN:REV-PLAN
   1 = if s" engine" else s" focused" then type cr ;

\ A throw code does not cross a process boundary, so the caught code leaves as
\ the exit status (lib/process.f PROC-EXIT-RC).
: MAIN ( -- )
   [: PLAN ;] catch {: rc:n :}
   rc 0= if exit then
   s" chain-plan: uncaught throw code " type rc . cr
   s" " rc CHAIN-PLAN:FAIL-RC PROC-EXIT-RC die ;

MAIN
;package
