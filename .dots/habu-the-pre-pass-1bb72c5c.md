---
title: The pre-pass defers a word nothing defines where the load refuses a bad stored signature
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:15:38.284481+02:00"
---

Problem: a renderer reached in one package, then in another ': W ( -- ) ; s" W" s" -- foo" trust' and a definition that fits and calls 'W NOPE': --verify-only exits 0 with W-CHECK-DEFERRED at NOPE, while the load refuses at the trust with E-BAD-STORED-SIGNATURE. Same on the engine before the unseen lane. Program b3 in /home/joel/Work/reviews/habu-10x-2026-11-10/cases8/unseen-refold2 (probe-b.txt). Found by the fold worker of habu-an-uncheckable-token-fc4ec7cb (/home/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/unseen-refold.md); not checked whether it is meant. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the pre-pass and the load agree on b3, or the document says why a stored signature naming an unknown type defers; a test pins it. Files: src/habu/verify-source.f, src/core/checker.f, test/diag-position-test.f. Verify: b3 under the load, the check and --verify-only. Depends: none. Ownership: the checker's unseen-name rule. Claim: unassigned.

2026-11-10 (bob): this defect is the quick check's alone (tools/check.f --verify-only, src/habu/verify-source.f), the scanner habu-check-src-through-1198f4ed replaces. It is fixed by that dot, not in the scanner, and is worked last in its section.
