---
title: instruction model misses live encoders and cites a phantom dot
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.061643+02:00"
---

Problem: formal/Common/Insn.v:316-378 models 62 forms; src/arch/arm64/asm.f ships 79 encoders; unmodelled and unrecorded: ENC-ORN (257), ENC-MVN (266), ENC-ASRV (395), ENC-ADRD (440) plus the 14 FP encoders (only the FP gap is in the MODEL GAPS paragraph :181-184); test/compiler/native-a64ir.f:1254-1258 cites dot habu-model-orn-in-39435de5 which does not exist; src/compiler/a64-effect.f:50-52 claims every bound is read off the modelled vocabulary while the chain emits a64.fadd..fcmp* and a64.mvn. Acceptance: Orn/Asrv/Adrd rows added (RRR/field forms like the modelled ones); the FP gap recorded with a real dot; the phantom citation replaced; the a64-effect.f sentence corrected. Files: formal/Common/Insn.v, test/compiler/native-a64ir.f, src/compiler/a64-effect.f. Verify: proof slice; decode_encode rows count. Depends: prover. Ownership: proofs. Claim: unassigned.
