---
title: Select shifted-index adds and msub
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T10:58:43.374662+02:00"
blocks:
  - habu-select-mask-literals-f082dbf3
  - habu-count-the-arm64-e13ae0a3
---

Campaign: ARM64 code-size fixes, design revision 3 (§3.4). Line references are at master 8c9b75af; re-verify before editing.

Problem: EXPAND-CELL-INDEX (src/compiler/native/elaborate.f:3415-3419) plus MUL-FOLD-FOR (select.f:1838-1847) make `cells +` a `movz #8; madd` pair; EXPAND-MODULO (elaborate.f:3433-3439) makes `mod` a `sdiv; mul; sub` triple; ENC-ADD has no shift field (src/arch/arm64/asm.f:269, RRR :144); ENC-MSUB exists unused by the selector (asm.f:299).

Acceptance: ENC-ADDSL and ENC-LDRX in asm.f with golden rows in test/compiler/insn-schema.f from an independent assembler; operations a64.addsl, a64.msub, a64.ldrx; SHL-FOLD-FOR before MUL-FOLD-FOR for an add of a single-use multiply by a single-use power-of-two constant; a sub whose second operand is a single-use multiply selects msub with checked operand order; a single-use addsl feeding only a cell load folds into ldrx (stores are never folded: DO-STORE is a word call). Tier-1 fixtures execute and count spans: runtime cell index (`add … lsl #3`, no madd, no movz #8), the same read (`ldr … lsl #3`), mod with a runtime divisor (`sdiv; msub`, no mul/sub), mod with a constant nonzero divisor, /mod with both results live, a product with a second use (unchanged), signs, extremes, `MIN-N -1 mod` = 0. Artifact: suite output; scaled-index and remainder census rows; added compiler bytes; tools/engine-size.f before and after; gen 2 == gen 3.

Break-even: dispatch only if the habu-count-the-arm64-e13ae0a3 census counts more than 200 sites across the three shapes (about 700 bytes of encoder, schema and rules).

Files: src/arch/arm64/asm.f, src/compiler/native/a64ir.f, select.f, emit.f, test/compiler/insn-schema.f, native-select.f, native-emit.f, native-exec.f.

Verify: tools/native-build.f product; bin/hb --load test/compiler/insn-schema.f; the three suites; census; tools/engine-size.f; tools/two-generation-build.f; bin/hb --load test/run.f. No engine text.

Depends: habu-select-mask-literals-f082dbf3 (shared select.f); habu-count-the-arm64-e13ae0a3's count.

Ownership: the files above.

Claim: unassigned.
