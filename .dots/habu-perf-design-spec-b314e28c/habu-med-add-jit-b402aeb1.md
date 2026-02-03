---
title: [MED] Add JIT microbench + triggers
status: closed
priority: 2
issue-type: task
created-at: "2026-02-03T12:45:06.752710+01:00"
closed-at: "2026-02-03T19:55:13.849671+01:00"
close-reason: bench-jit, tiering trigger, GC-safe const_pool
blocks:
  - habu-low-stack-maps-1d094863
  - habu-fix-repl-chunk-dd041c71
  - habu-fix-macro-chunk-5f196625
  - habu-fix-compiler-env-aec7d63b
  - habu-fix-gc-work-76dc7cfe
  - habu-ir-skeleton-d7b83105
---

src/interp/vm.zig + src/jit/jit.zig: no tiering/profiling; JIT not exercised in benchmarks. Fix: add bench/jit.zig that executes a hot bytecode loop with JIT enabled; add minimal hot-loop counter in vm (per-chunk or per-ip) to trigger compilation; report compile time + speedup. Verification: zig build bench -Doptimize=ReleaseFast.
