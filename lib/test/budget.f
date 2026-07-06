\ budget.f - load-aware test timeout budgets.
\
\ Fixed wall-clock budgets in process-spawning suites flake under box
\ saturation (concurrent gate runs): a healthy-but-slow child overruns a
\ fixed budget and throws E-PROC-TIMEOUT (dot
\ habu-concurrent-multi-workspace-5341c7f4). T-BUDGET-MS scales a nominal
\ budget by the load factor the gate measures at startup (TR-CALIBRATE
\ cal-factor, exported to every worker as HB_LOAD_PCT), so budgets track
\ measured load instead of guessing the wall clock. Detection stays BOUNDED:
\ the factor is clamped to [T-BUDGET-MIN-PCT .. T-BUDGET-MAX-PCT] - budgets
\ never shrink below nominal, and a genuinely hung child still fails within
\ T-BUDGET-MAX-PCT/100 (= 3x) of the nominal budget; the clamp mirrors the
\ gate's own TR-CAL-MIN-PCT/TR-CAL-MAX-PCT. Standalone runs (no HB_LOAD_PCT)
\ use the nominal budget unchanged.

require lib/errors.f
require lib/string.f

100 constant T-BUDGET-MIN-PCT
300 constant T-BUDGET-MAX-PCT
variable T-BUDGET-PCT   0 T-BUDGET-PCT !   \ 0 = not yet read from HB_LOAD_PCT

: T-BUDGET-CLAMP ( n -- n ) {: pct:n :}
   pct T-BUDGET-MIN-PCT < if T-BUDGET-MIN-PCT exit then
   pct T-BUDGET-MAX-PCT > if T-BUDGET-MAX-PCT exit then
   pct ;

\ Unset, empty, or non-numeric HB_LOAD_PCT means "no measured load": nominal.
: T-BUDGET-ENV-PCT ( -- n )
   s" HB_LOAD_PCT" GETENV STR>NUMBER? 0= if drop T-BUDGET-MIN-PCT exit then
   T-BUDGET-CLAMP ;

: T-BUDGET-INIT ( -- )
   T-BUDGET-ENV-PCT T-BUDGET-PCT ! ;

: T-BUDGET-MS ( n -- n )
   T-BUDGET-PCT @ 0= if T-BUDGET-INIT then
   T-BUDGET-PCT @ * 100 / ;
