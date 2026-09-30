---
title: Select mask literals and immediate shifts
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T10:58:43.364215+02:00"
blocks:
  - habu-drop-the-link-f2071a86
  - habu-count-the-arm64-e13ae0a3
---

Campaign: ARM64 code-size fixes, design revision 3 (§3.3). Line references are at master 8c9b75af; re-verify before editing.

Problem: MATERIALISE (src/compiler/native/select.f:1125-1135) never selects `ORR xd,xzr,#mask` although ORRI, ENC-ORRI and A64IR:MASK-IMM? exist (a64ir.f:561, src/arch/arm64/asm.f:478, a64ir.f:393); $FFFFFFFF is two instructions. lshift/rshift always select LSLV/LSRV (select.f:2744-2745) with a materialized count although ENC-LSLI/ENC-LSRI exist (asm.f:307-316).

Acceptance: new operations a64.movmask, a64.lsli, a64.lsri with schema rows; MATERIALISE takes the mask form for ADDR-NONE values whose MOVZ and MOVN costs both exceed 1 and that pass MASK-IMM?; the shift rules take the immediate form for a single-use ADDR-NONE constant count, effective count `k and 63`, zero binding the input with no instruction. Tier-1 fixtures execute and count spans: $FFFFFFFF (one instruction), $FF00FF00FF00FF00 (one), $7FFFFFFFFFFFFFFF (still one MOVN), $12345678 (unchanged chain), a pointer literal (unchanged carrier); shifts by 0, 1, 63, 64, 65, -1 of a runtime value agree with the engine and show one immediate shift and no lslv/lsrv. Artifact: suite output; mask-chain and constant-shift census rows; the added compiler bytes; tools/engine-size.f before and after; gen 2 == gen 3. The product must shrink net of the added rules (constant-shift folding 0fe1d896 was rejected at +1,208 bytes).

Break-even: dispatch only if the habu-count-the-arm64-e13ae0a3 census counts more than 150 sites across the two patterns (about 400-600 bytes of added rules and schema).

Files: src/compiler/native/a64ir.f, select.f, emit.f, test/compiler/native-select.f, native-emit.f, native-exec.f.

Verify: tools/native-build.f product; the three suites; census; tools/engine-size.f; tools/two-generation-build.f; bin/hb --load test/run.f. No engine text.

Depends: habu-drop-the-link-f2071a86 (shared select.f); habu-count-the-arm64-e13ae0a3's count.

Ownership: the files above.

Census (tools/codegen-census.f, product of 2f165004, SHA-256 82148a2d…8c25): mask-chain 97 sites (800 B, est. saving 412 B), constant-shift 147 sites (1,176 B, est. saving 588 B at most): 244 sites clear the 150-site break-even.

Claim: unassigned.
