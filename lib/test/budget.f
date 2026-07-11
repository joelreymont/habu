\ budget.f - load-aware test timeout budgets.
\
\ Fixed wall-clock budgets in process-spawning suites flake under box
\ saturation (concurrent gate runs): a healthy-but-slow child overruns a
\ fixed budget and throws E-PROC-TIMEOUT (dots
\ habu-concurrent-multi-workspace-5341c7f4, habu-process-test-standalone-
\ 9de825bc). T-BUDGET-MS scales a nominal budget by a measured load factor:
\ the gate measures once at startup (TR-CALIBRATE cal-factor) and exports it
\ to every worker as HB_LOAD_PCT; a STANDALONE run (no HB_LOAD_PCT) now
\ SELF-CALIBRATES with the same spin probe instead of assuming an idle box -
\ the proven flake class was exactly a standalone suite running nominal
\ budgets on a saturated machine. Detection stays BOUNDED: the factor is
\ clamped to [T-BUDGET-MIN-PCT .. T-BUDGET-MAX-PCT] - budgets never shrink
\ below nominal, and a genuinely hung child still fails within
\ T-BUDGET-MAX-PCT/100 (= 3x) of the nominal budget; the clamp mirrors the
\ gate's own TR-CAL-MIN-PCT/TR-CAL-MAX-PCT (run-lib reuses these constants
\ and the spin, so the two calibrations cannot drift apart).

require lib/errors.f
require lib/string.f
require lib/adt/option.f                 \ option<n> STR>NUMBER? consumer (switchover wave A)

100 constant T-BUDGET-MIN-PCT
300 constant T-BUDGET-MAX-PCT
1000000 constant T-BUDGET-NS-PER-MS
$A00000 constant T-BUDGET-CAL-ITERS
95 constant T-BUDGET-CAL-REF-MACOS-MS      \ idle-box spin wall time; 0 = target uncalibrated
variable T-BUDGET-PCT   0 T-BUDGET-PCT !   \ 0 = not yet initialised
variable T-BUDGET-CAL-SINK

: T-BUDGET-CLAMP ( n -- n ) {: pct:n :}
   pct T-BUDGET-MIN-PCT < if T-BUDGET-MIN-PCT exit then
   pct T-BUDGET-MAX-PCT > if T-BUDGET-MAX-PCT exit then
   pct ;

: T-BUDGET-CAL-SPIN ( n -- n )             \ the gate's fixed-work calibration spin
   0 swap begin dup 0 > while
      swap dup dup * drop 1 + swap
      1-
   repeat drop ;

: T-BUDGET-CAL-REF-MS ( -- n )
   HB-TARGET-MACOS? if T-BUDGET-CAL-REF-MACOS-MS exit then
   0 ;

: T-BUDGET-CAL-PCT ( n n -- n ) {: measured:n ref:n :}   \ measured, reference -> clamped factor
   ref 0 <= if T-BUDGET-MIN-PCT exit then
   measured 100 * ref / T-BUDGET-CLAMP ;

: T-BUDGET-SELF-PCT ( -- n )               \ measure the live box now (standalone runs)
   mono-ns {: t0:n :}
   T-BUDGET-CAL-ITERS T-BUDGET-CAL-SPIN T-BUDGET-CAL-SINK !
   mono-ns t0 - T-BUDGET-NS-PER-MS /
   T-BUDGET-CAL-REF-MS T-BUDGET-CAL-PCT ;

\ Unset, empty, or non-numeric HB_LOAD_PCT means "no gate-exported factor":
\ self-calibrate rather than assuming an idle box.
: T-BUDGET-ENV-PCT ( -- n )
   s" HB_LOAD_PCT" GETENV STR>NUMBER? MATCH option
     none OF T-BUDGET-SELF-PCT exit ENDOF
     some OF ENDOF
   ;MATCH
   T-BUDGET-CLAMP ;

: T-BUDGET-INIT ( -- )
   T-BUDGET-ENV-PCT T-BUDGET-PCT ! ;

: T-BUDGET-MS ( n -- n )
   T-BUDGET-PCT @ 0= if T-BUDGET-INIT then
   T-BUDGET-PCT @ * 100 / ;
