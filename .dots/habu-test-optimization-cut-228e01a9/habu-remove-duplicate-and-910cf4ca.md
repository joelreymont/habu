---
title: Remove duplicate and change-detector checker cases
status: closed
priority: 1
issue-type: task
created-at: "2026-09-29T18:03:11.957449+02:00"
closed-at: "2026-09-29T23:07:09.069751+02:00"
close-reason: "Landed 4408763d (review PASS; enum MAKE/UNMAKE twin added). Kept with reasons: type-ctor provider section, field-proj section 1 positives, type-export keyword-path rejects. Gate 503/503."
---

Problem: test/verify-prim-test.f EXPECT-HIDDEN asserts only code 70, which a defined-but-mismatched reference also gives (DIAG-BUF never read); duplicates and stand-ins: type-ctor mock-provider section (real path enum-decl section 20), structure-make sections 1-7 vs structure-certify, type-export sections 5/8 vs export-package, pointer-storage vs typed-storage section 8, multi-error-api vs checker-effect-authority, field-proj positives vs structure-decl DERIVE addr; change detectors: typed-storage-structural section 1 pins open holes as passing, self-comparisons in structure-make section 8 and type-family-rollback section O, retired-name tombstones in type-field-owner (13 forks). Evidence with lines: ~/.cache/tmp/kestrel-gate/test-review/L4-checker.md. Acceptance: verify-prim asserts the hidden-reference diagnostic itself (mutation: mismatched reference fails it); each duplicate removed with its surviving twin named; change detectors removed. Files/Ownership: the test/ files named, not test/field-proj-boundary.f or test/prop-test.f. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. Depends: none. Claim: unassigned.
