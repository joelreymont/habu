---
title: Define GC parity gates
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-20T08:55:19.437133+01:00\""
closed-at: "2026-02-20T09:22:48.381886+01:00"
close-reason: completed
blocks:
  - habu-build-habu-vs-5f1f7bf6
---

File: tools/gc-compare:1, tools/perf-loop:1, bench/check.zig:1; cause: parity target is not machine-enforced; fix: define pass/fail thresholds for avg/p95 pause, throughput, RSS; why: objective convergence loop.
