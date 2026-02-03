---
title: [MED] Implement real GC benchmark
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-03T12:44:55.099981+01:00\\\"\""
closed-at: "2026-02-03T18:13:31.539572+01:00"
close-reason: Implemented real GC bench w/ avg+p95, bytes_copied/live_bytes + --json; wired bench args; zig build test
blocks:
  - habu-fix-repl-chunk-dd041c71
  - habu-fix-macro-chunk-5f196625
  - habu-fix-compiler-env-aec7d63b
  - habu-fix-gc-work-76dc7cfe
  - habu-ir-skeleton-d7b83105
---

bench/gc.zig:1: bench prints text only; no pause/throughput numbers. Fix: allocate a controlled live graph, run N collections, report avg/p95 pause + bytes_copied + live_bytes; add --json flag. Update build.zig bench step docs. Verification: zig build bench -Doptimize=ReleaseFast.
