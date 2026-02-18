---
title: Write barrier stores
status: open
priority: 1
issue-type: task
created-at: "2026-02-18T21:50:53.697209+01:00"
blocks:
  - habu-nursery-layout-scaffold-7aa479dc
---

src/interp/vm.zig and src/runtime/primitives/*.zig mutators. Cause: no old->young tracking exists. Fix: add card-mark barrier in all pointer stores (cons/vector/hash/class/slot writes). Why: correctness for generational collection.
