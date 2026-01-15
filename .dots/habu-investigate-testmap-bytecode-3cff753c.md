---
title: Investigate testmap bytecode disassembly
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T19:19:45.733102+02:00"
---

src/bytecode/disasm.zig: Add debug to print disassembled testmap closure bytecode. Need to verify if child chunk has correct load_global instructions or if bytecode itself is corrupted during emit.
