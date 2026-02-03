---
title: [MED] Add VM interpreter microbench
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-03T12:45:01.431143+01:00\\\"\""
closed-at: "2026-02-03T18:45:19.197701+01:00"
close-reason: Add VM microbench + bench-vm step
blocks:
  - habu-fix-repl-chunk-dd041c71
  - habu-fix-macro-chunk-5f196625
  - habu-fix-compiler-env-aec7d63b
  - habu-fix-gc-work-76dc7cfe
  - habu-ir-skeleton-d7b83105
---

bench/: missing VM-level benchmarks. Fix: add bench/vm.zig that runs: fixnum loop, consing loop, hash-set/get loop, string concat loop; measure ops/s and allocations/GC count via heap.stats. Verification: zig build bench -Doptimize=ReleaseFast; compare before/after.
