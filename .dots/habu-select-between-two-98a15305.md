---
title: Select between two doubles
status: active
priority: 2
issue-type: task
created-at: "2026-08-04T11:26:51.051954+02:00"
---

Problem: src/compiler/native/select.f admits an if-conversion only when every value the join takes is a cell; R-WIDTH-OK? refuses a join carrying a double, so a selection such as `x f0< if 0.0 else x then` keeps its branch. The machine form is FCSEL Dd,Dn,Dm,cond and src/arch/arm64/asm.f has no encoder for it. Acceptance: ENC-FCSEL plus its mnemonic, its rows in test/compiler/insn-schema.f and its constructor in formal/Common/Insn.v when the floating vocabulary reaches the model, an a64.fcmpfsel/a64.fselz pair in src/compiler/native/a64ir.f, and the REAL? refusal in R-WIDTH-OK? removed with test/compiler/native-select.f SELECT-REAL-CASE turned from a refusal into a conversion. Files: src/arch/arm64/asm.f, src/arch/arm64/mnem.f, src/compiler/native/a64ir.f, src/compiler/native/emit.f, src/compiler/native/spill.f, src/compiler/native/select.f, test/compiler/native-select.f. Verify: bin/hb --load test/compiler/native-select.f, native-a64ir.f, native-emit.f, native-chain.f, insn-proof.f, tools/codegen-compare.f. Depends: none. Ownership: the a64 dialect and the arm64 assembler. Claim: agent=fcsel workspace=.jj-ws/habu-select-between-two-98a15305
