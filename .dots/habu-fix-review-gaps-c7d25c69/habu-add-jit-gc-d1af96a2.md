---
title: Add JIT GC root API
status: open
priority: 1
issue-type: task
created-at: "2026-02-01T22:29:57.335579+01:00"
---

Context: src/jit/ctx.zig, src/runtime/heap.zig:1433, src/interp/vm.zig:619; cause: GC roots exclude JIT stack; fix: plumb JitContext roots into GC entrypoints; deps: none; verification: add GC relocation test
