---
title: Reset inherited fs cleanups in forked children
status: open
priority: 2
issue-type: task
created-at: "2026-08-08T22:57:18.098582+02:00"
---

A forked child inherits lib/fs-mutate.f's FS-MUT-CLEANUP table and CLEANUP-RUN (fs-mutate.f:289) walks ALL of it: a gate pool member that runs its own cleanups (lib/object-index-test.f:90 CLEANUP-RUN) also executes the DRIVER's 'GT-ROOT CLEANUP-TREE+' registration (test/run-lib.f TR-START, default-TMPDIR arm only) and deletes the gate capture root under every sibling - E-FS-OPEN/E-FS-IO with a rotating victim in the stdlib/lint-libs/core group. Measured: full gate red 3/3 with default TMPDIR, green 3/3 with HB_TMP set (which registers no root cleanup); members green standalone and via the gate-stdlib.f child. Pre-existing fork defect exposed by the 53 newly scheduled dark suites (schedlint chain, parked behind this fix). Invariant to repair: cleanup registrations are process-owned; after fork a child must start with an EMPTY table (its own registrations only) - the parent's entries describe the parent's ownership and running them in a live parent's lifetime is always wrong. Fix at the fork child entry (lib/process-fork.f child dispatch or test/gate-pool.f worker prologue - pick the owning seam, prefer the fork library so every fork user is covered), NOT per-test guards. Regression test: a forked child that registers+runs its own cleanup must not delete a path the parent registered; drive it through the real pool (test/gate-pool.f), not a re-implementation. Verify: the full gate greens on the DEFAULT temp root 3x with the schedlint chain applied. Files: lib/process-fork.f or test/gate-pool.f, lib/fs-mutate.f (only if the table needs a fork hook), a test beside the pool's own. Depends: none.
