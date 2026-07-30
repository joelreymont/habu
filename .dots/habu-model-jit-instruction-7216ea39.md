---
title: Model JIT instruction encodings in Rocq
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-30T09:22:08.306131+02:00\""
---

Full context: PRIORITY 1 proof upgrade, directed 2026-07-30. The engine's assembler words in src/habu/habu2.f (MOVZ, MOVK, LDR, STR, ADD/SUBI, LSLI/LSRI/ASRI, AND/ORR, CMP, BCOND, B, BL, CBZ/CBNZ, RET, and the rest of the emitted vocabulary - enumerate from the source, do not guess) have no correctness statement: a field-packing mistake ships silently until a crash. Build formal/Common/Insn.v: an inductive of the emitted instruction forms with their operand fields, an encode function to 32-bit words matching the ARM64 encodings, a decode for the same subset, and theorems: decode after encode is identity on the vocabulary; field ranges refuse out-of-range operands the same way the shipped words do (compare against their guards); distinct forms encode distinctly. Bind with a parity gate (test/compiler/insn-proof.f following the checker-model-proof.f conventions): ONE shared vector table of (form, operands, expected 32-bit word) drives BOTH the Rocq obligations AND a Habu test that calls the REAL assembler words emitting into a scratch buffer and compares the emitted word - measure first how the emitter words write (CP-relative? buffer cell?) and use the real emission path, not a reimplementation (Test Integrity). Falsify by mutation: flip one field shift in one shipped assembler word, rebuild, the gate must red on exactly that form's rows; restore. Start with the forms the snapshot relocation pass depends on (BL, LDRB/STRB, LSLV, ORR) so this composes with the relocation round-trip proof (sibling dot), then grow to the full emitted vocabulary; record uncovered forms as MODEL GAPS rather than stopping the leaf.

Claim: agent=insnproof workspace=.jj-ws/habu-model-jit-instruction-7216ea39
