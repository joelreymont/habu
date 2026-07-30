---
title: Guard x18 in LDAR, STLR, CBZ and CBNZ
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-30T10:43:29.739027+02:00\""
---

Full context: found while building the instruction-encoding parity gate (dot habu-model-jit-instruction-7216ea39). src/arch/arm64/asm.f opens by promising that the Darwin-reserved register x18 is refused at encode time 'for every X-register operand field', and XREG? does that for the shifted-register, immediate, load/store, move-wide, compare and indirect-branch encoders. Four emitted forms escape it. ENC-LDAR and ENC-STLR (src/arch/arm64/asm.f, the two lines that read '5 lshift or $C8DFFC00 or MSK') never call XREG? at all. CBZ, and CBNZ, in src/arch/arm64/icode.f build their word directly from $B4000000/$B5000000 rather than going through ENC-CBZ/ENC-CBNZ, so the XR2ND check those encoders do carry never runs; ENC-B, ENC-BL, ENC-BCOND, ENC-CBZ and ENC-CBNZ are in fact dead in the native path and exist only for parity with the Gforth seed generator in bootstrap/cg/asm.fs. Evidence: test/compiler/insn-schema.f rows F-LDAR 18 15, F-LDAR 14 18, F-STLR 18 5, F-CBZ 18, F-CBNZ 18 all emit a word and exit 0, while every guarded slot exits 72; formal/Common/Insn.v proves unguarded_x_register_forms, which says these four are exactly the modelled forms whose checked operand list differs from their X-register list. Fix: route the acquire/release encoders through XR2 and give the icode branch emitters the same check (either by calling the asm.f encoders or by adding XREG? at the icode site), then flip those five schema rows from an emitted word to exit code 72 and update checked_regs in the model so unguarded_x_register_forms becomes 'no form'.

Claim: agent=asmguards workspace=.jj-ws/habu-refuse-out-of-3536f1ed
