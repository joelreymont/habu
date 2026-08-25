---
title: Widen the section-reach lint to every emitter file
status: open
priority: 2
issue-type: task
created-at: "2026-08-15T14:08:04.286209+02:00"
---

MINOR from the bake-spine destruction review: tools/aot-section-reach-lint.f enforces the no-ADR-into-the-AOT-section rule over ONE hardcoded file (habu2.f, in MAIN and T-REAL-FILE) - the section labels are DECLARED in habu1.f, and habu1.f/snap-lib.f also emit engine code; an ADR into a section label added there is invisible. Probed clean today (zero such sites) - a scope gap, not a live hole. Fix: derive the scanned set from the files that emit engine code, structurally. Also from the same review: the aot-wide-format CONTAINS? prefix-fragility (prewin-calls 1 matches prewin-calls 11 - use the REPORT= span comparison) and the icode-fixup comment naming AOT-WINDOW-AGREE where the word is AOT-SECTION:AGREE. Files: tools/aot-section-reach-lint.f, test/aot-wide-format-suite.f, test/icode-fixup-test.f. Depends: none.
