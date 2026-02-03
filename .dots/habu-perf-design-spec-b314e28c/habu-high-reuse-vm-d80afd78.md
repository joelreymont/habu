---
title: [HIGH] Reuse VM GC root buffer
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-03T12:44:42.462219+01:00\""
closed-at: "2026-02-03T15:38:09.906512+01:00"
close-reason: Reuse VM GC roots buffer
blocks:
  - habu-fix-repl-chunk-dd041c71
  - habu-fix-macro-chunk-5f196625
  - habu-fix-compiler-env-aec7d63b
  - habu-fix-gc-work-76dc7cfe
---

src/interp/vm.zig:726: collectGarbageExtra allocates ArrayList roots each GC -> allocator churn + possible GC failure. Fix: store reusable root buffer in Vm; clearRetainingCapacity; pre-reserve for STACK_SIZE+globals+frames; ensure zero allocations in steady-state GC. Add stress test: repeated GC under load. Verification: zig build test.
