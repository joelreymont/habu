---
title: Model the logical-immediate mask synthesis in Rocq
status: open
priority: 3
issue-type: task
created-at: "2026-07-30T10:44:08.215824+02:00"
---

Full context: MODEL GAP recorded while building the instruction-encoding parity gate (dot habu-model-jit-instruction-7216ea39). ANDI,, ORRI, and EORI, take a plain 64-bit mask and call >LIMM in src/arch/arm64/asm.f to turn it into the packed N:immr:imms the encoding carries. formal/Common/Insn.v models the packing - the Andi/Orri/Eori forms take the packed 13-bit value as their operand and the gate proves the field lands at bits 10..22 - but not >LIMM itself, which is a real algorithm: LELEM finds the smallest repeating element by halving, POPC64 counts its ones, LROT searches for the rotation that reproduces it, and LIMM-PACK assembles the three pieces, with 0 and all-ones refused. Today the two are bound only by the four example rows in test/compiler/insn-schema.f (masks $2000000000000000, $FF, 1 and $FFFF) plus the two refused masks, all driven through the real >LIMM. What is missing is the general statement: a Rocq function for encodeBitMasks with the theorems that a returned packing decodes back to the mask it was given, that a refusal means no packing exists, and that the packing is unique. Then the example rows become instances of it rather than the whole of it.
