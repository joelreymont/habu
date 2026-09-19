---
title: Encode x86-64 padding and undefined traps
status: open
priority: 2
issue-type: task
created-at: "2026-09-19T18:30:02.209902+03:00"
---

Reconciliation c191257a: experiment NOP and UD2 forms are missing from X64ASM. Add the single-byte padding instruction and explicit undefined-instruction trap to the existing BUF encoder for layout and fail-closed emission. Pin llvm-mc vectors nop=90 and ud2=0f0b. Ownership: hazel, src/arch/x86-64/asm.f and test/compiler/x86-64-asm.f.
