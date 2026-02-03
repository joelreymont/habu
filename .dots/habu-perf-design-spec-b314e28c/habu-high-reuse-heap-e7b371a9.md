---
title: [HIGH] Reuse heap GC roots buffer
status: open
priority: 1
issue-type: task
created-at: "2026-02-03T12:44:49.070036+01:00"
blocks:
  - habu-fix-repl-chunk-dd041c71
  - habu-fix-macro-chunk-5f196625
  - habu-fix-compiler-env-aec7d63b
  - habu-fix-gc-work-76dc7cfe
---

src/runtime/heap.zig:1461: collectGarbage builds all_roots ArrayList every GC (external roots + intern tables + packages + readtables) -> allocator churn + can fail while trying to GC. Fix: keep a reusable Value buffer in Heap (or GC) and refill each GC with clearRetainingCapacity; avoid allocations during trace; add test that runs multiple GC cycles and asserts no backing_allocator growth after warmup. Verification: zig build test.
