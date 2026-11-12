---
title: Make the ARM64 assembler refuse catchably
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T18:10:54.802016+02:00"
---

Every operand guard in src/arch/arm64/asm.f (?REG, ?IMM16, ?IMM12, ?NIS, ?SHIFT, ?COND, ?HW, SCALE/, XREG?, LIMM-BAD) ends the process with 'ASM-EXIT-RC die' (exit 72, asm.f:17). That is right for the build-time emitter it was written for, and wrong for the JIT: on any failure the compile transaction must release its builders, discard the object bytes, restore the dictionary, data and code marks, emit a stage-specific diagnostic and leave no published word. None of that can happen after die. src/compiler/native/emit.f correctly does not duplicate the assembler's bounds (one authority per field), so a hand-built A64IR module with an out-of-field move-wide operand kills the process instead of being refused, and no test can assert that refusal in process. Wanted: the guards throw a named code, with the existing exit-72 behaviour kept for the build-time callers by catching at their boundary, so both the standalone builder and the JIT get the failure mode each needs. Check every caller in src/arch/arm64/icode.f, src/habu/ and tools/ before changing the shared guards.
