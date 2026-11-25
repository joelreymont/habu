---
title: Load the x86 disassembler only for DEBUG
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T17:47:58.485119+01:00"
---

Problem: src/lib/debug.f:13 requires src/arch/x86-64/disasm.f on x86 alone, so a program that keeps a DEBUG word carries 22 KB of code and tables that ARM's does not. Acceptance: an app that keeps the same DEBUG words is no larger on x86 for the disassembler. Depends: none. Claim: unassigned.
