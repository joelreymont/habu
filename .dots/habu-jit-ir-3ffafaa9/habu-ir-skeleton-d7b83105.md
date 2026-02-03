---
title: IR skeleton
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-03T13:29:05.961130+01:00\\\"\""
closed-at: "2026-02-03T16:28:44.664603+01:00"
close-reason: Add SSA IR skeleton + verifier
blocks:
  - habu-fix-repl-chunk-dd041c71
  - habu-fix-macro-chunk-5f196625
  - habu-fix-compiler-env-aec7d63b
  - habu-fix-gc-work-76dc7cfe
---

src/jit/ir.zig: define SSA core: BlockId, ValueId, Inst, Type, Func, cfg edges; basic builder API; no codegen yet. Add verifier stubs.
