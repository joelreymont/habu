---
title: Speed up forwarding target validation
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-24T18:13:38.314159+01:00\\\"\""
closed-at: "2026-02-24T18:32:43.703602+01:00"
close-reason: "Rejected after profiling/A-B: no proven workload win; reverted objects changes"
---

RCA from /tmp/ratsimp40_jit_run.sample: runtime.objects.forwardingTargetLooksValid (src/runtime/objects.zig:956+) dominates GC scan time via std.mem.isAligned/alignForward debug-heavy helpers. Replace with branch-light power-of-two bit math helpers (overflow-safe) and keep layout invariants. Add unit coverage for helper math and forwarding-size guard.
