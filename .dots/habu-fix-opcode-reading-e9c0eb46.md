---
title: Fix opcode reading in patchChunkIndices
status: open
priority: 2
issue-type: task
created-at: "2026-01-17T08:30:40.255822+02:00"
---

src/interp/repl.zig:36 patchChunkIndices reads opcodes as 2 bytes but they are 1 byte. Change from reading u16 to u8. This caused invalid enum panics when patching chunks.
