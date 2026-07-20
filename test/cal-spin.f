\ cal-spin.f - fresh-child calibration probe for the full-gate perf verdict.
\
\ Prints the wall-clock milliseconds of ONE run of the gate's fixed calibration
\ spin (lib/test/budget.f T-BUDGET-CAL-SPIN) in a freshly spawned process, then
\ exits. test/run-lib.f TR-CAL-CHILD-MS? spawns this and parses the number.
\
\ Why a fresh child: the perf verdict brackets each attempt with a pre-run and a
\ post-run calibration spin and invalidates the attempt on >10% drift between
\ them. The spin is pure integer work with zero in-process drift (20 back-to-back
\ spins in one process all measure the same ms), so the drift is NOT process-state
\ growth. On a heterogeneous host (DGX Spark GB10: 10 Cortex-X925 performance
\ cores at 3.9GHz measure the spin at 87ms, 10 Cortex-A725 efficiency cores at
\ 2.8GHz measure it at 132ms) the LONG-LIVED gate driver launches on a
\ performance core (pre-cal = 87ms) but, while it blocks idle waiting on the pool
\ workers, the scheduler migrates it onto an efficiency core. Its single in-process
\ post-cal spin then STARTS on that efficiency core and, being CPU-bound, migrates
\ back to a performance core partway through - so it reads a blended ~104ms and
\ trips the >10% drift check every run. A freshly spawned CPU-bound child inherits
\ the driver's full (unpinned) affinity mask and is placed by the scheduler on a
\ performance core from its first instruction, so its spin reads 87ms and matches
\ the pre-cal placement on every host. See test/run-lib.f TR-POST-CAL! /
\ TR-CAL-CHILD-MS? and skills/habu-host-profiles/SKILL.md.

require lib/test/budget.f
require lib/fmt.f

package CAL-SPIN

variable SINK

public

: MS ( -- n )                                \ wall-ms of one fixed calibration spin, this fresh process
   mono-ns {: t0:n :}
   T-BUDGET-CAL-ITERS T-BUDGET-CAL-SPIN SINK !
   mono-ns t0 - T-BUDGET-NS-PER-MS / ;

;package

CAL-SPIN:MS FMT:.U cr
