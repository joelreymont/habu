---
title: Refuse out-of-range ARM64 operands at encode time
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T10:43:48.990732+02:00"
---

Full context: found while building the instruction-encoding parity gate (dot habu-model-jit-instruction-7216ea39). The encoders in src/arch/arm64/asm.f bound no operand at all: every field is shifted and OR-ed into the word, so an operand one past its field runs into the neighbouring one and the wrong instruction is emitted with no diagnostic. Four proved counterexamples, all committed as rows in test/compiler/insn-schema.f and as theorems in formal/Common/Insn.v: MOVZ, x0 with a 16-bit immediate of 65536 emits $D2A00000, which is 'movz x0, #0, lsl #16'; CSET, x1 with condition 16 emits $9A9F17E1, which is 'cset x1, eq'; ADDI, x1 x2 with 4096 emits $91400041, which is not any instruction the model names; ADD, x1 x2 with register 32 emits $8B200041, likewise. Two more come from the scale divisions: MOVK, divides its shift by 16 and LDR,/STR, divide their byte offset by 8 with a plain Forth /, so a shift of 8 silently becomes 0 and an offset of 12 silently becomes 8. Fix: add fail-closed range checks beside XREG? - a register field 0..31, imm16 0..65535, imm12 0..4095, the packed logical immediate 0..8191, a shift 0..63, a condition 0..15 - and make the scaled load/store and move-wide mnemonics refuse a byte operand that is not a multiple of its scale, all through the same s" ..." 72 die path the reserved-register check uses. Then move those rows in test/compiler/insn-schema.f from the overflow table to the reserved table with code 72, and turn the three counterexample theorems in formal/Common/Insn.v into refusal statements.
