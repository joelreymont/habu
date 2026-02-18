---
title: GC regression+perf gates
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-18T21:50:53.724832+01:00\\\"\""
closed-at: "2026-02-18T23:24:44.603064+01:00"
close-reason: added generational GC bench/check regression gates
blocks:
  - habu-minor-gc-collector-2f89a428
---

bench/gc.zig and bench/check.zig. Cause: perf gate exercised semispace-only behavior and missed generational regressions. Fix: run GC bench in generational mode with promotion+LOS activity and enforce structural invariants (promoted bytes, old-space liveness, bounds) in bench-check. Why: prevent silent GC regressions while iterating.
