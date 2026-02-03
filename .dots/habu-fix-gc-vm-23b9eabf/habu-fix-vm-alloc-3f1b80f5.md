---
title: Fix VM alloc rooting
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-03T10:40:11.183832+01:00\\\"\""
closed-at: "2026-02-03T12:32:57.544317+01:00"
close-reason: Root VM alloc args across GC
blocks:
  - habu-fix-vm-gc-00648d03
---

src/interp/vm.zig:432-529 + opcode handlers: allocCons/allocClosureWithGC run GC but don't keep/update Value args/captures across collection. Fix: ensure args are in roots across GC (stack-based or extra-roots) and refreshed before retry; audit all opcode paths that allocate after popping values. Add stress test with tiny heap that forces GC during CONS/LIST/closure creation. Verification: zig build test.
