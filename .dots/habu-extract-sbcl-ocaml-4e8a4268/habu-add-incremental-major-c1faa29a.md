---
title: Add incremental major marking
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-20T08:55:19.476780+01:00\""
closed-at: "2026-02-20T14:26:58.112872+01:00"
close-reason: Completed incremental major GC mark/sweep/barrier/pause validation
blocks:
  - habu-implement-gc-debt-bb3f3f6e
---

File: src/runtime/gc.zig:1; cause: monolithic major cycles cause long pauses; fix: sliced major mark steps integrated with allocation safepoints; why: p95 parity requires incremental major work.
