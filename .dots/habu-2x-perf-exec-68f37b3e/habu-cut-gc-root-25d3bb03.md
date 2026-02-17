---
title: Cut GC root overhead
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-17T11:08:27.558392+01:00\\\"\""
closed-at: "2026-02-17T12:57:56.234790+01:00"
close-reason: "completed: removed gc_vals mirror roots; slot-based GC roots"
blocks:
  - habu-rebaseline-perf-post-b340b0e2
---

Optimize collectGarbageExtra root assembly and eliminate avoidable root-copy churn. Files: src/interp/vm.zig:1066-1196, docs/stack-maps.md. Target: reduce avg_pause_ns from ~102ms by >=2x on bench --json baseline.
