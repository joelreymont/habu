---
title: clobber-lint cannot see a packaged label
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T22:01:29.226022+02:00"
---

tools/lint/clobber-lint.f:98 START-L? asks whether the FIRST character of the label token is `l`, so a package-qualified label (PROT:LOPEN, SNAP-RELOC:LCALLS, HIDX:LREBUILD, AOT-WINDOW:LDATA, DEFER-DIAG:LDEFNOTOKEN, HOLD-EMIT:LHOLDQ, KWDATA:LKWTRUSTRAW) fails it however its tail is spelled. The predicate is shared by COLLECT-OPENINGS (routine bodies) and CALLEE? (call sites), so such a label is not merely uncounted in the census - its routine is never analysed and its callers never see its clobbers. Every packaged label in the tree has been invisible since packaging started. It surfaced when the protection narrowing (dot habu-narrow-the-code-291b2cef) moved 47 call sites into `package PROT` and the call census fell 504 -> 458; that lanes MIN-CALLS row was lowered to 458 with the derivation recorded in the source, so this dot owns restoring the coverage.

FIX, MEASURED TO WORK on 2026-08-11 in the stageb lane before being reverted: read the segment after the LAST colon instead of the first character. With that change alone the census reads routines 335 -> 347, calls 504 -> 518, and two CLOBBER findings appear (c-esdq x11 via lbcs, c-local-ref x0 via lvspill). Both are FALSE POSITIVES of the union model, and both go away by adding two PRESERVE-MASK rows the model already supports: LCEMIT preserves x0/x1/x2/x8/x12/x13/x16/x30 (its window-miss path frames x30 and x1 around a PROT:LGROW that frames the rest), and PROT:LGROW preserves x0/x2/x8/x16/x30. With those rows the lint reads clean at routines 347, calls 518.

WHY IT WAS NOT DONE THERE: package-diff-lint refuses it. START-L?, PSEUDO?, PSEUDO-EFFECTS, PRESERVE-MASK in tools/lint/clobber-lint.f, every CLT-* word in tools/lint/clobber-lint-test.f, and the new fixture words are all bare globals, so E-PACKAGE-OWNERSHIP requires the whole legacy file to be packaged first. That is the real prerequisite and the reason this is its own leaf: package clobber-lint.f (LESSONS 2026-08-11 says EXPORT NAME inside a public section is the tool for a large legacy file - definitions stay private in place and the export list reads in one block), then apply the predicate fix and the two preserve rows.

REGRESSION TO KEEP, also written and measured in that lane: append to tools/lint/clobber-sys-x8-fixture.f a callee whose label lives in a package (`package CLOBBER-FIX public variable LQUAL ;package`) plus a caller that keeps x13 live across `CLOBBER-FIX:LQUAL LABEL@ BL,`. The fixture count goes 3 -> 4 findings and the census 13/6 -> 15/7. Falsified by mutation: restoring the first-character predicate drops the finding back to 3 and the test dies. Add direct assertions too - START-L? true for LPROT, PROT:LOPEN, SNAP-RELOC:LCALLS and A:B:LTAIL, false for LHEAD:OPEN, PROT:OPEN, PROT: and CP.

Files: tools/lint/clobber-lint.f, tools/lint/clobber-lint-test.f, tools/lint/clobber-sys-x8-fixture.f. Depends: none.
