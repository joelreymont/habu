---
title: Verify hash-table-alist primitive exists
status: open
priority: 2
issue-type: task
created-at: "2026-01-18T06:26:24.611846+02:00"
---

Files: src/compiler/compile.zig:8603, src/bytecode/opcodes.zig
Verify hash_alist opcode is defined and emitted correctly.
Test that (hash-table-alist table) works in test suite.
Verify: grep finds opcode definition, test passes.
Est: 15min
