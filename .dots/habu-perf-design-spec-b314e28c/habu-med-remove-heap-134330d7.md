---
title: [MED] Remove heap.allocCons in op args
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-03T12:45:13.330741+01:00\\\"\""
closed-at: "2026-02-03T17:13:08.817388+01:00"
close-reason: GC-safe consing for op args; add regression
blocks:
  - habu-fix-repl-chunk-dd041c71
  - habu-fix-macro-chunk-5f196625
  - habu-fix-compiler-env-aec7d63b
  - habu-fix-gc-work-76dc7cfe
  - habu-ir-skeleton-d7b83105
---

src/interp/vm.zig:1675+: many op handlers build arg lists via self.heap.allocCons (no GC) and can OOM without recovery. Fix: switch to self.allocCons (GC-enabled) and keep intermediate Values rooted (stack slots or collectGarbageExtra); add regression that forces OOM while building args list. Verification: zig build test.
