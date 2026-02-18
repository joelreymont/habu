---
title: GC regression+perf gates
status: active
priority: 1
issue-type: task
created-at: "\"2026-02-18T21:50:53.724832+01:00\""
blocks:
  - habu-minor-gc-collector-2f89a428
---

bench/gc.zig and bench/check.zig. Cause: perf gate exercised semispace-only behavior and missed generational regressions. Fix: run GC bench in generational mode with promotion+LOS activity and enforce structural invariants (promoted bytes, old-space liveness, bounds) in bench-check. Why: prevent silent GC regressions while iterating.
