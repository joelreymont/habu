---
title: Package the native-defer-image fixtures
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T19:36:44.704359+03:00"
---

Problem: `test/native-defer-image.f`'s child programs define `DEFER-IMAGE-NATIVE`, `DEFER-IMAGE-JIT`, `DEFER-IMAGE-FRESH` and `DEFER-IMAGE-INSTALL` as globals. Forth-card section 8 wants test fixtures packaged, as the same children already package `DEFER-IMAGE-LOOP` and `DEFER-IMAGE-CHECK`. This predates batch 13; the tier-1 fix review found it.
Acceptance: each child's own words live in a package, and the suite's expected output is unchanged.
Files: `test/native-defer-image.f`.
Verify: the native-defer-image suite on spark.
Depends: none.
Ownership: krait.
Claim: unassigned.
