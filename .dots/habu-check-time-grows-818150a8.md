---
title: "Check time grows faster than linearly in a definition's width"
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T16:21:44.177067+02:00"
---

Problem: on master a definition of 5000 literals takes 7.5 s to check and one of 50000 runs past 120 s; the cause is not known. Found by the review of habu-a-type-mismatch-0d7ded80. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: measured again once habu-a-refused-def-fe5ca1f0 has landed (it changes when chains are shortened); if it still grows faster than linearly, a hunt names the cause as path:line with a measurement, then the time grows linearly and a case bounds it by CPU-time ratio. Files: src/core/checker.f. Verify: 5000, 20000 and 50000 literals, accepted and refused. Depends: habu-a-refused-def-fe5ca1f0. Ownership: the checker's walk. Claim: unassigned.

Also 2026-11-10: an accepted definition checks superlinearly on master too: ok40k.f doubled to 80,000 tokens takes 7.29 s against about 2.6 s at 40,000.
