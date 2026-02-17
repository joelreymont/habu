---
title: Fix hoist compile blocker
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-17T11:08:27.555360+01:00\""
closed-at: "2026-02-17T12:57:58.952595+01:00"
close-reason: "completed: hoist+habu build/test/bench smoke passes on latest hoist"
blocks:
  - habu-rewire-jit-eligibility-699cbe9e
---

Resolve current hoist dependency compile failure in ../hoist/src/regalloc/liveness.zig:474 (OperandCollector type mismatch) so zig build test and perf work can run.
