---
title: Add JIT mod
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-03T22:24:32.249220+01:00\\\"\""
closed-at: "2026-02-03T22:26:16.673632+01:00"
close-reason: Implement Op.mod in JIT
---

Context: src/jit/jit.zig:129 + src/jit/rt.zig; cause: JIT lacks Op.mod, forcing blacklist + VM fallback; fix: add rt.mod (callBinaryWithGc arith.mod) + compileOp .mod -> emitBinaryCall(rt.mod); add parity case (mod 10 3); deps: habu-parity-tests-9be195f5; verification: zig build test.
