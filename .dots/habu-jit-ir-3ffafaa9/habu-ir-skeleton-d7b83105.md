---
title: IR skeleton
status: open
priority: 1
issue-type: task
created-at: "2026-02-03T13:29:05.961130+01:00"
blocks:
  - habu-fix-repl-chunk-dd041c71
---

src/jit/ir.zig: define SSA core: BlockId, ValueId, Inst, Type, Func, cfg edges; basic builder API; no codegen yet. Add verifier stubs.
