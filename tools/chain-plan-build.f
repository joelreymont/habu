\ chain-plan-build.f - CLI entry for the checked chain planner.

require tools/chain-plan.f

package CHAIN-PLAN-CLI

: MAIN ( -- )
   SCRIPT-ARGC 2 <> if s" chain-plan: revision and repository root are required" 64 die then
   0 SCRIPT-ARGV$ 1 SCRIPT-ARGV$ CHAIN-PLAN:REV-PLAN
   1 = if s" engine" else s" focused" then type cr ;

MAIN
;package
