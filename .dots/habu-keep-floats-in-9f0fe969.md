---
title: Keep floats in the float file
status: active
priority: 2
issue-type: task
created-at: "2026-08-07T13:36:13.433742+02:00"
---

Claim: agent=fpplace workspace=.jj-ws/habu-keep-floats-in-9f0fe969

Audit: ~100 gap bytes and the maki-relevant class — chain values live in GPRs and every FP op pays fmov bridges in and out (RELU-F 8, MAX-F 8, SGD 16 incl. a dead duplicate), FP constants build via GPR mov;movk;fmov (12 bytes) where fmov d,#imm8 encodes small constants directly (SEG-1, FROUND materialises 0.5 TWICE), and fmul;fsub stays unfused where clang emits fmsub. Three pieces: float values whose producers and consumers are all FP ops get FPR-resident placement (the two-file allocator machinery exists — file-keying landed); FMOV-immediate selection for imm8-encodable constants (check the encoder ships it; Insn row if the model lacks it); FMA contraction in the combine pass (fmadd/fmsub need Insn rows first — rows-first discipline, cite the SMULH landing as the shape). The float corpus rows are the witnesses; T-SUM's non-associativity pin held in the audit and must keep holding — no reassociation, contraction only where the answer is bit-identical (fmsub rounds once; the differential decides per row).
