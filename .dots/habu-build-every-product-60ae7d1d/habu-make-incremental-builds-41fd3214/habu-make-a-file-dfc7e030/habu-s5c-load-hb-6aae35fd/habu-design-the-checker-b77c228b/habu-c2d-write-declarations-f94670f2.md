---
title: "C2d: write declarations' facts and move the forward PRIM: rows"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T20:16:59.603881+02:00"
blocks:
  - habu-c2c-store-a-1bfc2ab6
---

Problem: design r2 D4: every writer of checker facts (definers, declarations, PRIM: rows) finds its record through the dictionary, and the 125 forward PRIM: rows name words not yet defined. Acceptance: each declaration writer stores its facts through CHECK:FACTS!; the 125 forward PRIM: rows move below their definitions; the checker's separate stores for those facts go. Files: src/core/checker.f, the files holding the forward rows. Verify: candidate build; bin/hb --load test/run.f. Depends: C2c. Ownership: those files. Claim: unassigned.
