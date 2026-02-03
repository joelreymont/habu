---
title: [MED] Remove heap.allocCons in op args
status: open
priority: 2
issue-type: task
created-at: "2026-02-03T12:45:13.330741+01:00"
blocks:
  - habu-fix-repl-chunk-dd041c71
---

src/interp/vm.zig:1675+: many op handlers build arg lists via self.heap.allocCons (no GC) and can OOM without recovery. Fix: switch to self.allocCons (GC-enabled) and keep intermediate Values rooted (stack slots or collectGarbageExtra); add regression that forces OOM while building args list. Verification: zig build test.
