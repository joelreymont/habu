---
title: [HIGH] Reuse VM GC root buffer
status: open
priority: 1
issue-type: task
created-at: "2026-02-03T12:44:42.462219+01:00"
blocks:
  - habu-fix-repl-chunk-dd041c71
---

src/interp/vm.zig:726: collectGarbageExtra allocates ArrayList roots each GC -> allocator churn + possible GC failure. Fix: store reusable root buffer in Vm; clearRetainingCapacity; pre-reserve for STACK_SIZE+globals+frames; ensure zero allocations in steady-state GC. Add stress test: repeated GC under load. Verification: zig build test.
