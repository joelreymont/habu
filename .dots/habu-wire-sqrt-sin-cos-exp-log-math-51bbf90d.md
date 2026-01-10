---
title: Wire sqrt/sin/cos/exp/log math functions
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-10T18:00:32.159170+02:00\""
---

Primitives exist in arith.zig but need to be wired:
- Add IR nodes in ir.zig (~line 456)
- Add symbol interning in compile.zig
- Add dispatch in compile.zig
- Add opcodes in opcodes.zig
- Add emission in emit.zig
- Add VM execution in vm.zig
