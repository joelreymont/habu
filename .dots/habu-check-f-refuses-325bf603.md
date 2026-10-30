---
title: "check.f refuses test/name-length-test.f: bare TRUST"
status: open
priority: 3
issue-type: task
created-at: "2026-10-30T16:06:33.064585+02:00"
---

Problem: hb --load tools/check.f -- test/name-length-test.f throws 7158 E-VS-BARE-TRUST (src/habu/verify-source.f:59, raised :2844-2845) at TRUST (:489 on the lane 6 copy, :490 on master ba954d5c): the source preverifier requires TRUST to follow two string literals (name, signature), and the test passes them from words (CEIL$ SIG$ TRUST). Pre-existing on master. Found by the lane 6 worker and its reviewer, 2026-10-30. Acceptance: either the test passes literals or the preverifier admits a TRUST whose operands come from words, whichever the trust rule (docs/forth-card.md, verify-source.f header) intends; check.f rc 0 on the file. Files: test/name-length-test.f, src/habu/verify-source.f. Verify: check.f rc 0. Depends: none. Ownership: verify-source preverifier. Claim: unassigned.
