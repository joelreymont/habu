---
title: Package the source assembler gate and unit-test LIMM?
status: open
priority: 2
issue-type: task
created-at: "2026-08-07T14:08:38.160116+02:00"
---

Problem: tools/asm-src-test.f is one of the legacy unpackaged tools/*-test.f files (38 are packaged). Adding a word to it trips E-PACKAGE-OWNERSHIP in tools/package-diff-lint.f, so the LIMM? predicate minted in src/arch/arm64/asm.f (habu-extend-the-immediate-e3e8378f) could not get direct unit rows there. Its behaviour IS proven through the production fold path in test/compiler/native-combine.f (MHOLE/MZERO/MALL refuse a non-contiguous mask, a zero mask and an all-ones mask), but the asm-level edge rows were dropped rather than refactor a legacy file with global TRUST declarations mid-lane. Acceptance: tools/asm-src-test.f opens a real package per docs/forth.md, its global TRUST rows keep working, and AST-TEST-LIMM gains direct LIMM? rows including 0, -1, 5 (non-contiguous and SMALLER than an encodable mask), 0xFFFF00FF, -2, and the agreement rows pinning LIMM? true against the nis >LIMM builds. Files: tools/asm-src-test.f. Verify: bin/hb --load tools/asm-src-test.f; jj diff --git > $HB_TMP/change.diff then bin/hb --load tools/package-diff-lint.f -- $HB_TMP/change.diff. Depends: none. Ownership: tools/asm-src-test.f. Claim: unassigned.
