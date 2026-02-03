---
title: Add JIT cons
status: active
priority: 1
issue-type: task
created-at: "\"2026-02-03T22:08:28.126158+01:00\""
---

Context: src/jit/jit.zig:129 + src/jit/rt.zig:91 + src/tests/jit_parity.zig:40; cause: JIT lacks Op.cons lowering so parity suite avoids (cons ...); fix: add compileOp .cons -> emitBinaryCall(rt.cons) + add rt.cons using allocConsWithGc; restore (cons ...) cases in jit_parity; deps: none; verification: zig build test.
