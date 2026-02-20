---
title: "Major GC: barrier-assisted marking"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-20T08:58:56.356729+01:00\\\"\""
closed-at: "2026-02-20T14:13:40.160575+01:00"
close-reason: completed
blocks:
  - habu-major-gc-incremental-068b1148
---

src/runtime/gc.zig, src/interp/vm.zig, src/jit: add write-barrier support for incremental major correctness.
