---
title: Mark satellite checker scratch spans
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T15:50:44.513538+02:00"
blocks:
  - habu-hold-per-def-8d5c52f1
---

Problem: CHECKER-REG:SCRATCH{ ... }SCRATCH (src/core/checker.f) marks the checker's working cells as scratch, and only checker.f uses it: the temporaries of the satellite checker files (src/core/type-family.f, render.f, sumtype.f) are plain variables outside any span. A count on 2026-10-30 found about 100 checker temporaries outside spans (its list is lost). They need one rest mechanism, not one span line each. The top-level typed row (src/core/top-row.f) survives require and : by design, so it is not scratch.
Acceptance: every checker temporary sits in a span or the one mechanism; a self-build is measured before and after.
Depends: typed top-level storage. Claim: unassigned.
