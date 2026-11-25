---
title: "Save the checker's candidate cells in one place"
status: open
priority: 3
issue-type: task
created-at: "2026-11-25T17:13:54.044256+01:00"
---

Problem: CHECK-CANDIDATE!, CHECKER-PREFIX:BIND? and REQUIRED-WALK (src/core/checker.f, src/core/check-walk.f) each save and restore nearly the same set of checker cells, so a cell added to one can be missed in the others. Acceptance: one word holds the set and the three use it; check-family suites pass. Depends: the require feature's landing. Claim: unassigned.
