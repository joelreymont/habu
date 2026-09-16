---
title: Model the writeback load/store forms in the insn proof
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T19:47:48.898060+03:00"
---

Problem: src/arch/arm64/asm.f gained six writeback encoders (ENC-LDRPRE/LDRPOST/STRPRE/STRPOST, ENC-LDRDPRE, ENC-STRDPOST, 727aab75) that the engine and both tiers now emit on every stack move, and formal/Common/Insn.v does not model them, so the instruction parity gate covers a smaller subset of what ships than before (ENC-ORN, ENC-MVN and the FP set were already outside it). Acceptance: the four integer writeback forms and the two D-register forms are constructors in Insn.v with a decode round-trip lemma each, rows in test/compiler/insn-schema.f, insn-cases.f, insn-obligations.f and insn-axioms.txt, and the parity gate green; the objdump-verified vectors in test/compiler/a64-indexed.f are the goldens. Files: formal/Common/Insn.v, test/compiler/insn-*.f, insn-axioms.txt. Verify: the formal build (formal/ Makefile) and the parity suites. Depends: none. Ownership: formal model. Claim: unassigned.
