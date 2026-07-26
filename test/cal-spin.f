\ cal-spin.f - fresh-child calibration probe for the full-gate perf verdict.
\
\ Prints the wall-clock milliseconds of ONE run of the gate's fixed calibration
\ spin (test/cal-spin-lib.f CAL-SPIN:MS) in a freshly spawned process, then
\ exits. test/run-lib.f TEST-RUN:CAL-CHILD-MS? spawns this and parses the number.
\ The measurement itself lives in test/cal-spin-lib.f because the quiescent
\ ratchet phase (test/json-read-perf-phase.f) brackets its own run with the same
\ spin and the shared drift tolerance requires the same number.
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
\ TEST-RUN:CAL-CHILD-MS? and skills/habu-host-profiles/SKILL.md.

require test/cal-spin-lib.f
require lib/fmt.f

CAL-SPIN:MS FMT:.U cr
