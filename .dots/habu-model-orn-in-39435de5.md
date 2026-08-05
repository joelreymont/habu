---
title: Model Orn in the instruction vocabulary
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T11:29:45.009900+02:00"
---

src/arch/arm64/asm.f gained ENC-ORN (Orr with the shifted-register N bit set) and ENC-MVN (Orn with the zero register) so that Habu's invert compiles to one instruction, and src/compiler/native/a64ir.f's a64.mvn names that form. Orn is NOT in the 48-form vocabulary formal/Common/Insn.v models and test/compiler/insn-proof.f pins, so it is the one machine form of the native dialect that the instruction parity gate does not cover: add F-ORN to test/compiler/insn-schema.f, an ORN, mnemonic in src/arch/arm64/mnem.f, encoding and reserved-register rows in test/compiler/insn-proof.f, and the matching constructor and encode clause in formal/Common/Insn.v with its Rocq parity proof. Until then the encoder is held by an assertion in test/compiler/native-a64ir.f against the shipped assembler's own output, and by the executed invert fixture in test/compiler/native-vocab.f. Filed while landing habu-complete-the-comparison-63760034.
