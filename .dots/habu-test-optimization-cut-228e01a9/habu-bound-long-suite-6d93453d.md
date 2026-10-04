---
title: Bound long suite rows by CPU time
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T23:31:35.906184+02:00"
---

Problem (lane 369 r4-loaddead, cd2ec4d0): build rows are now bounded by CPU time (GT-POOL-CPU-BUDGET!, PROC-TREE:CPU-NS) with a wall hang guard, but suite rows still end on wall time under load: test/gate-stdlib-lib.f:14 SUITE-TIMEOUT-MS 360 s and their own wall deadlines (test/c2-memory-e2e.f:16 600 s) turned c2-memory, c2-view-record, c2-field-loan and build-fixpoint-snapshot into TIMEOUT-UNDER-LOAD reds in loaded gates though each passes alone; tools/native-build-core.f:35 SMOKE-TIMEOUT-MS 10 s wall inside every build can fail a starved smoke run. Acceptance: every gate row's pass/fail bound is its own CPU use (budget per row from measured CPU with headroom) with a wall hang guard, the inner wall deadlines of the long rows bounded the same way or by the row's guard, the smoke run bounded by CPU or by a guard that load cannot reach; a saturated gate (64 hogs) runs those rows to PASS; a hung row is still killed; docs/gate.md states it. Files: test/gate-stdlib-lib.f, test/gate-pool.f, test/c2-*-e2e.f, tools/build-fixpoint*.f, tools/native-build-core.f, docs/gate.md.
