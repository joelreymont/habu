---
title: Fix REPL chunk roots
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-03T10:40:19.794376+01:00\\\"\""
closed-at: "2026-02-03T13:45:14.662261+01:00"
close-reason: Use persistent chunk pool pointers
blocks:
  - habu-fix-vm-alloc-3f1b80f5
---

src/interp/repl.zig:516-524: vm.setChunkPool(chunk_ptrs.items) points at temp ArrayList freed on return; persistent_chunks (Value list) not registered as GC roots. Fix: make chunk_pool storage stable (store pointers persistently or clear/reset after run) and set vm.ext_roots to include persistent_chunks/items + current chunk during eval; ensure ext_roots restored. Add regression: compile nested closure, trigger GC, then eval another form using old chunks. Verification: zig build test.
