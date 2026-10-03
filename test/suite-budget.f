\ suite-budget.f - the CPU budget and hang guards of a native gate suite row.
\
\ BOUNDED BY ITS OWN WORK, as a build row is (test/keyed-image.f). The gate
\ ends a suite row once its process tree has run CPU-MS of CPU time, user and
\ system, whatever the load (test/gate-stdlib-lib.f ROW-BUDGET!,
\ test/gate-pool.f GT-POOL-CPU-BUDGET!). The row's wall deadline in the pool,
\ ROW-MS, is then only a hang guard: a row that stopped running gains no CPU
\ time, so wall time alone ends it. A long row gives each process it starts
\ CHILD-MS, five times the budget, so a child within the budget reaches its
\ deadline only on less than a fifth of a core. The row's guard is a minute
\ more, so a child hung from the row's start meets its own deadline, and the
\ row names the step, before the pool ends the row. The longest row,
\ c2-memory, runs 113 s of CPU; docs/gate.md has the measurements.
\
\ A module of its own, so that a row file takes its children's deadline without
\ loading the gate.

package SUITE-BUDGET

public

360000 constant CPU-MS
CPU-MS 5 * constant CHILD-MS
CHILD-MS 60000 + constant ROW-MS

;package
