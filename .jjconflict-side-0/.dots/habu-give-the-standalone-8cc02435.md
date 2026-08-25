---
title: Give the standalone ARM64 assembler checked stack effects
status: open
priority: 3
issue-type: task
created-at: "2026-07-30T10:44:08.227299+02:00"
---

Full context: recorded while building the instruction-encoding parity gate (dot habu-model-jit-instruction-7216ea39). src/arch/arm64/asm.f, src/arch/arm64/icode.f and src/arch/arm64/mnem.f are written in the standalone's Forth with no stack effects, because the Gforth recovery compiler in tools/bootstrap.sh has to read them before the native checker exists (see the note at the top of src/arch/arm64/icode.f about keeping stage-source words local-free). Every checked caller therefore has to declare their effects by hand: test/compiler/insn-schema.f carries 58 TRUST lines, and tools/asm-src-test.f carries another 14, which is two copies of the same interface that can drift from each other and from the code. The boundary is named and tested - the parity gate drives every one of those words and would fail loudly if an effect were wrong - but it is still an unchecked seam in the middle of the code generator. Fix: work out what the recovery compiler actually needs (it may be that only icode.f has the locals restriction, or that the restriction can be lifted now that the native fixpoint is the normal path), then give the assembler real CHECKED: effects and delete both TRUST blocks. Until then, any new caller must reuse package COMPILER-INSN-PROOF rather than write a third copy.
