---
title: Green the remaining compiler and checker suites
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T10:57:56.513026+03:00"
---

Problem: on the complete run of 2026-09-12 (engine 04701ef9, LESSONS.md red set) these suites are red for causes not yet attributed to a family, exit code and first failure: compiler-ir-structure-manifest (1: F3); compiler-ir-structure-proof (1: F3); compiler-asm-package (1: F6); compiler-native-elaborate (1: F298); compiler-codegen-tail-probe (1: F13); match-factor-pin (1: F5); type-ctor (70: habu: in zps:sone: at 'sone', LOWER-WIDTH-AWARE-ROUNDTRIP); engine (1: F304, F309, habu-fix-the-two-284ac502). Acceptance: each attributed to a cause (its own dot when a family emerges) and green on the root engine, or its case retired with the reason in the commit and the LESSONS.md red set updated in the same commit. Files: the suite files named in test/gate-stdlib-cases.f, src/compiler for the compiler-* suites. Verify: each suite standalone on the root engine, then test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
