---
title: CHECK-CANDIDATE! reads a line break between body tokens as no break
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:47:35.854022+02:00"
---

Problem: src/core/checker.f CHECK-CANDIDATE! walks raw text its caller supplies, and the walk's TBREAK? ends a body token at byte 32 only, while TBLANK? ends the head's first token at any byte at or below 32. Observed with a probe: s\" CC2 ( n -- n )\n1+" CHECK-CANDIDATE! answers 1, not -1. In the tree it is reached only by tests and by sumtype.f's generated single-spaced texts, so no load path shows it. Acceptance: a candidate text whose body tokens are separated by a line break or a tab checks as the same text with spaces. Files: src/core/checker.f. Verify: a case in test/signature-nesting.f beside its PACKET cases. Depends: none. Ownership: unassigned. Claim: unassigned.
