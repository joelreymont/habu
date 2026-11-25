---
title: Load the x86 disassembler only for DEBUG
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T17:47:58.485119+01:00"
---

Problem: src/habu/debug.f:14 requires src/arch/x86-64/disasm.f on x86 alone, so hb and a `--repl` program carry 21,804 bytes of code and tables that ARM's do not; a stripped program carries none. Acceptance: an x86 `--repl` program is no larger for the disassembler than ARM's. Depends: none. Claim: unassigned.
