---
title: Perf/design/spec parity plan
status: open
priority: 2
issue-type: task
created-at: "2026-02-03T12:44:17.305934+01:00"
blocks:
  - habu-parity-tests-9be195f5
  - habu-bench-harness-f817afd6
  - habu-med-add-jit-b402aeb1
  - habu-low-stack-maps-1d094863
  - habu-fix-repl-chunk-dd041c71
  - habu-fix-macro-chunk-5f196625
  - habu-fix-compiler-env-aec7d63b
  - habu-fix-gc-work-76dc7cfe
  - habu-ir-skeleton-d7b83105
---

Context: repo-wide perf+design+ANSI CL parity review. Evidence: src/interp/vm.zig:726 (VM GC roots alloc); src/runtime/heap.zig:1461 (heap GC roots alloc); src/runtime/gc.zig:120 (work_list peak bug); bench/gc.zig:1 (placeholder bench); docs/cranelift-parity.md:1 (JIT gaps); docs/cl-symbols.md:5 + 886 (audit counts + stubs). Goal: add measurement harness + close perf/correctness gaps; keep dots blocked until habu-fix-repl-chunk-dd041c71.
