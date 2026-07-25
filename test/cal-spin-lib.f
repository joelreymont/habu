\ cal-spin-lib.f - the gate's shared fixed calibration spin measurement.
\
\ CAL-SPIN:MS is the one measurement the perf tolerance is written against: the
\ wall-clock milliseconds of ONE run of the fixed spin in lib/test/budget.f
\ (T-BUDGET-CAL-SPIN). Two callers need it and they need the SAME number, so it
\ lives here rather than in either of them:
\   - test/cal-spin.f, the fresh-child probe test/run-lib.f spawns and parses
\     for the full-gate verdict's post-run calibration;
\   - test/json-read-perf-phase.f, which brackets its own quiescent ratchet
\     measurement with a spin before and after and compares the two through
\     PERF-VERDICT:DRIFT-OK?.
\ A second copy of this word would let the two drift apart silently and make the
\ shared ten-percent tolerance mean two different things.
\
\ Why the caller matters, not just the number: the spin is pure integer work
\ with zero in-process drift, but on a heterogeneous host (DGX Spark GB10:
\ Cortex-X925 performance cores measure it at ~87ms, Cortex-A725 efficiency
\ cores at ~132ms) WHERE the measuring process is scheduled decides what it
\ reads. test/cal-spin.f exists so the gate driver measures from a freshly
\ spawned CPU-bound child instead of from its own migrated core. See
\ test/cal-spin.f and test/run-lib.f TR-CAL-CHILD-MS?.

require lib/test/budget.f

package CAL-SPIN

variable SINK

public

: MS ( -- n )                                \ wall-ms of one fixed calibration spin, this process
   mono-ns {: t0:n :}
   T-BUDGET-CAL-ITERS T-BUDGET-CAL-SPIN SINK !
   mono-ns t0 - T-BUDGET-NS-PER-MS / ;

;package
