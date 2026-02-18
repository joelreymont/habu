---
title: VM/JIT barrier hooks
status: open
priority: 1
issue-type: task
created-at: "2026-02-18T21:50:53.720358+01:00"
blocks:
  - habu-write-barrier-stores-2b8bf449
---

src/jit/backend.zig and src/interp/vm.zig. Cause: JIT fast paths bypass future barrier logic. Fix: emit/store barrier helpers for compiled stores and safepoints before GC entry. Why: keep JIT/interpreter semantics identical.
