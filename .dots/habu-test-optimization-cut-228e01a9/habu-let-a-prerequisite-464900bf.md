---
title: Let a prerequisite image build finish under load
status: open
priority: 1
issue-type: task
created-at: "2026-10-02T19:10:52.902812+02:00"
---

Problem (Tim's measurement via dave, /private/tmp/claude-501/wsland2/w3/table.txt; round-4 int7 gate the same): under heavy load the whitebox-engine-build row is killed at its wall deadline (test/gate-images.f BUILD-ROW-TIMEOUT-MS = WHITEBOX-KEY:BUILD-TIMEOUT-MS 360 s + 60 s; test/keyed-image.f BUILD-TIMEOUT-MS 240 s for the other families) though the uncapped build succeeds in 482-486 s; every WHITEBOX-SUITE row then refuses exit 75 without running (57 rows in the int7 gate), so machine load becomes dozens of reds that say nothing about the tree. Suite rows under load hit TIMEOUT-UNDER-LOAD the same way (c2-memory, c2-view-record, c2-field-loan, build-fixpoint-snapshot at 360 s). Acceptance: a build row other rows wait on is bounded by its own work, not wall time under saturation: measure the child's CPU time (rusage) against a CPU deadline and keep a wall deadline only as a hang guard sized for the saturated pool (or extend the wall deadline while the pool reports saturation, if CPU accounting is unavailable); a forced saturated run (pool of N CPU hogs) where the build exceeds 360 s wall but stays under its CPU budget completes and its dependents run; a genuinely hung build (sleep, no CPU) is still killed by the hang guard; docs/gate.md states the rule. Files: test/gate-images.f, test/keyed-image.f, test/whitebox-key.f, test/gate-pool.f, docs/gate.md.
