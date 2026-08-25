---
title: Model the ARM64 floating-point encoders in Rocq
status: open
priority: 3
issue-type: task
created-at: "2026-07-30T10:43:49.005167+02:00"
---

Full context: MODEL GAP recorded while building the instruction-encoding parity gate (dot habu-model-jit-instruction-7216ea39). formal/Common/Insn.v covers the 48 integer, branch and system forms the engine emits, but not the fourteen double-precision encoders in src/arch/arm64/asm.f: ENC-FMOVXD, ENC-FMOVDX, ENC-FMOVDD, ENC-FADD, ENC-FSUB, ENC-FMUL, ENC-FDIV, ENC-FNEG, ENC-FABS, ENC-FSQRT, ENC-FCMP, ENC-FCMP0, ENC-SCVTF and ENC-FCVTZS. They are all RR or RRR shapes over the D-register file, so they fit the existing e_rr/e_rrr helpers, the existing decoder row record, and the existing tactics without new machinery; what they need is fourteen constructors, fourteen rows whose (mask, value) pairs still pass all_excl, wf clauses (the D-register operands take no reserved-register check because x18 is an X register), and encoding vectors in test/compiler/insn-schema.f cross-checked against clang. Note while doing it that ENC-FMOVXD checks only its X operand and ENC-FCVTZS only its X destination, which is correct but should be recorded in checked_regs. There is no golden test for these encoders anywhere in the tree today, so this is the first thing that would catch a field slip in them.
