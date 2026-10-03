\ suite-budget.f - the CPU budget and hang guards of a native gate row: a
\ keyed image's build row and a suite row alike.
\
\ BOUNDED BY ITS OWN WORK. The gate ends a row once its process tree has run
\ CPU-MS of CPU time, user and system, whatever the load (test/gate-pool.f
\ GT-POOL-CPU-BUDGET!; test/gate-images.f BUILD, test/gate-stdlib-lib.f
\ ROW-BUDGET!). The row's wall deadline in the pool, ROW-MS, is then only a
\ hang guard: a row that stopped running gains no CPU time, so wall time alone
\ ends it. A builder (test/keyed-image.f BUILD-RUN, and
\ test/whitebox-engine.f's, test/cold-engine.f's and test/native-unit-image.f's)
\ and every process a long suite row starts take CHILD-MS, five times the
\ budget, so a child within the budget reaches its deadline only on less than a
\ fifth of a core. The row's guard is a minute more, for a build row's key
\ hashing and copy, and so that a child hung from the row's start meets its own
\ deadline, and the row names the step, before the pool ends the row. The
\ longest build, the unsealed engine's, runs 77 s of CPU and the longest suite
\ row, c2-memory, 113 s; docs/gate.md has the measurements.
\
\ A module of its own, so that a row file takes its children's deadline without
\ loading the gate.

package SUITE-BUDGET

public

360000 constant CPU-MS
CPU-MS 5 * constant CHILD-MS
CHILD-MS 60000 + constant ROW-MS

;package
