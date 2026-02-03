---
title: [MED] Add VM interpreter microbench
status: open
priority: 2
issue-type: task
created-at: "2026-02-03T12:45:01.431143+01:00"
blocks:
  - habu-fix-repl-chunk-dd041c71
  - habu-fix-macro-chunk-5f196625
---

bench/: missing VM-level benchmarks. Fix: add bench/vm.zig that runs: fixnum loop, consing loop, hash-set/get loop, string concat loop; measure ops/s and allocations/GC count via heap.stats. Verification: zig build bench -Doptimize=ReleaseFast; compare before/after.
