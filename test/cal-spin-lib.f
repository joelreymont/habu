\ cal-spin-lib.f - the gate's shared fixed calibration spin measurement.
\
\ CAL-SPIN:MS is the one measurement the perf tolerance is written against: the
\ wall-clock milliseconds of ONE run of the fixed spin in lib/test/budget.f
\ (T-BUDGET-CAL-SPIN). Two callers need it and they need the SAME number, so it
\ lives here rather than in either of them:
\   - test/cal-spin.f, the standalone probe used to derive and check a host
\     profile's spin reference;
\   - test/json-read-perf-phase.f, which brackets its own quiescent ratchet
\     measurement with a spin before and after and compares the two through
\     DRIFT-OK? below.
\ A second copy of this word would let the two drift apart silently and make the
\ shared ten-percent tolerance mean two different things.
\
\ Why the caller matters, not just the number: the spin is pure integer work
\ with zero in-process drift, but on a heterogeneous host (DGX Spark GB10:
\ Cortex-X925 performance cores measure it at ~87ms, Cortex-A725 efficiency
\ cores at ~132ms) WHERE the measuring process is scheduled decides what it
\ reads. That is why the ratchet phase takes its own readings in the worker that
\ is about to run the workloads, on the core it will run them on, and why
\ test/cal-spin.f measures from a freshly spawned child. See test/cal-spin.f.

require lib/test/budget.f

package CAL-SPIN

variable SINK

100 constant HUNDRED
10 constant DRIFT-PCT                        \ max tolerated drift, percent of the pre reading

public

: MS ( -- n )                                \ wall-ms of one fixed calibration spin, this process
   mono-ns {: t0:n :}
   T-BUDGET-CAL-ITERS T-BUDGET-CAL-SPIN SINK !
   mono-ns t0 - T-BUDGET-NS-PER-MS / ;

\ A BRACKET is two MS readings taken around a measurement. This package owns the
\ spin, so it owns the rule for when a pair of them still describes one machine:
\ a second copy of the ten-percent band living somewhere else would let the two
\ meanings drift apart silently, which is the same reason MS itself lives here.
\ The band is inclusive - exactly ten percent is still stable, one part past it
\ is not - and it is measured against the PRE reading, the one the budgets were
\ derived from.
: DRIFT-OK? ( n n -- bool ) {: pre:n post:n :}
   post pre - {: d:n :}
   d 0 < if d negate else d then {: ad:n :}
   ad HUNDRED *  pre DRIFT-PCT *  <= ;

;package
