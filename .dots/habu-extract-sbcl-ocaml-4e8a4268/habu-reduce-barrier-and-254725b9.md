---
title: Reduce barrier and safepoint overhead
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-20T08:55:19.490902+01:00\""
closed-at: "2026-02-20T15:39:32.492293+01:00"
close-reason: Barrier/safepoint overhead reduction dots completed
blocks:
  - habu-improve-los-policy-bfcc62a6
---

File: src/interp/vm.zig:1, src/jit/backend.zig:1, src/runtime/heap.zig:1; cause: write barrier/safepoints add mutator overhead; fix: inline fast paths, avoid redundant marks, hoist checks in JIT; why: throughput parity.
