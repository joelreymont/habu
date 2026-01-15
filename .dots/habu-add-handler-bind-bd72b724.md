---
title: Add handler-bind opcode
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:37:46.421930+02:00"
---

src/bytecode/opcodes.zig: Add handler_bind opcode ~0xB9 (after invoke_restart 0xB8). Add to Opcode enum and handlers array. Dependencies: none. Verify: opcode compiles.
