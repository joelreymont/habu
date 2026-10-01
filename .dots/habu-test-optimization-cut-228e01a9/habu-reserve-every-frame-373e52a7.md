---
title: Reserve every frame the arm64 contract admits
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T05:01:50.208211+02:00\""
closed-at: "2026-10-01T17:06:11.558300+02:00"
close-reason: Fixed by mykwqlzo 935fc9b5 (review 94 ACCEPT)
---

Problem: src/compiler/native/emit.f:487-493 WORD-RESERVE and WORD-RELEASE adjust sp by FRAME-SIZE with one ENC-SUBI / ENC-ADDI, a 12-bit immediate (at most 4095), while src/arch/arm64/machine.f:102 FRAME-MAX-N admits frames up to 32752 bytes (the scaled load/store reach) and A64RA sizes frames up to it (regalloc.f:724). Any frame from 4096 to 32752 bytes passes the allocator and dies in the assembler (src/arch/arm64/asm.f:61 ?IMM12, rc 72, uncatchable). Measured by the r4-nest lane: a 23-level nested ?do with a variable counter needs more than 4095 bytes and dies; 22 levels need 3920. Acceptance: every frame the contract admits encodes, links and runs (sp restored exactly, slot accesses in range); the encoding choice follows the emitter's invariants (instruction count per IR op, branch offsets, any other sp adjustment by FRAME-SIZE such as emit.f:887); the 24-level nest row and a direct large-frame case compile and run, written first and seen to die rc 72; native build convergence and two-generation build. Files: src/compiler/native/emit.f (and the machine contract only if it states a limit the encoder cannot meet). Verify: test/compiler/native-plusloop.f, the native loop and frame rows, convergence. Depends: habu-measure-and-fix-2295dfb6 (same lane, second commit). Ownership: sp adjustment encoding.
