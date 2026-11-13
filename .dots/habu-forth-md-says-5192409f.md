---
title: forth.md says a covered name in another package is still refused
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T15:00:01.988188+02:00"
---

Problem: docs/forth.md :2460-2461 says a use from another package is still refused, but a qualified use of a name a package's public mark covers defers or yields at PM:MADE (cases w2 and w3). The suite pins the same: test/signature-nesting.f:425-432 marks SN-MARKED's public words, and COVERED-CASES (:405), run from SN-UNMARKED, treats SN-MARKED:MADE as a name the run may define. Either the sentence or the engine is wrong: the mark's reach across packages is not stated anywhere else. Found by the re-review of habu-an-uncheckable-token-fc4ec7cb. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the document and the engine agree for a qualified use of another package's covered name, with a test for w2 and w3. Files: docs/forth.md, src/core/checker.f, test/signature-nesting.f. Verify: w2, w3 under the load, the check and --verify-only. Depends: habu-an-uncheckable-token-fc4ec7cb. Ownership: the checker's unseen-name rule. Claim: unassigned.

Reading 2026-11-10 (fold worker, not run): in UNSEEN-COVERS? a qualified name whose package has no public mark falls through to the current package's marks, so a qualified NOPE inside a marked package would count as covered. The acceptance cases gain one for it.
