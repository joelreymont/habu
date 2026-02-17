---
title: Raise JIT coverage
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-17T11:08:27.553507+01:00\\\"\""
closed-at: "2026-02-17T12:37:52.340444+01:00"
close-reason: completed
blocks:
  - habu-rebaseline-perf-post-b340b0e2
---

Expand Hoist translation eligibility for closures/HOF/float/string/hash workloads. Files: src/interp/repl.zig:2081-2112, src/jit/backend.zig translator cases. Target: convert interpreter-only benchmarks in baseline_v8.json to JIT.
