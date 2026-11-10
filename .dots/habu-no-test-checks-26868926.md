---
title: No test checks a MATCH opened at control frame 30
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:07:44.393551+02:00"
---

Problem: test/type-match-suite.f:232-233 checks a MATCH at frames 31 (E-MATCH-DEPTH) and 29 (certifies) and not at 30, the last depth that certifies (checker.f:21872, CF-FRAMES-MAX 2 -). Found by the uncheckable-reason re-review, which ran 29, 30 and 31 by hand. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the suite has the frame-30 case. Files: test/type-match-suite.f. Verify: the file passes; the case fails with the bound lowered by one. Depends: habu-tokens-refused-as-e0ec22ab landed. Ownership: the checker's tests. Claim: unassigned.
