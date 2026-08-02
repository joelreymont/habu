---
title: Prove the float instruction encodings in the shared insn rows
status: open
priority: 3
issue-type: task
created-at: "2026-08-03T01:29:19.078812+02:00"
---

test/compiler/insn-cases.f drives every pinned row through the shipped mnemonic in src/arch/arm64/mnem.f and reads the emitted word back out of the real code buffer, so a row that passes proves the shipped encoder produced that number. Not one FLOAT mnemonic is in it: FADD, FSUB, FMUL, FDIV, FNEG, FABS, FSQRT, FCMP, FCMP0, SCVTF, FCVTZS, FMOVXD, FMOVDX and FMOVDD are all absent from test/compiler/insn-proof.f's rows. Their encodings are proved today only end to end - a migrated body runs and answers what the interpreted body answers - which catches a wrong encoding but does not say which field was wrong. What is needed: one row per float mnemonic in the frozen row set, both register fields varied so a swapped Rn/Rd is visible, and the D-file register bound so a reserved-register refusal has a row too. Owners: COMPILER-INSN-PROOF, COMPILER-INSN-CASES.
