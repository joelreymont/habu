---
title: Add incremental major marking
status: open
priority: 1
issue-type: task
created-at: "2026-02-20T08:55:19.476780+01:00"
blocks:
  - habu-implement-gc-debt-bb3f3f6e
---

File: src/runtime/gc.zig:1; cause: monolithic major cycles cause long pauses; fix: sliced major mark steps integrated with allocation safepoints; why: p95 parity requires incremental major work.
