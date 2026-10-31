---
title: "Reuse definers.f's DEF-CLOSE and DEF-ABORT warm"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T21:55:04.674325+02:00"
---

Finding (EXT Rev 3 worker, carl-fext): an app that loads src/habu/definers.f compiles DEF-CLOSE and DEF-ABORT live on every warm build: 'ordered-dependency reason=primitive-state-unknown dependency=def-close'. Acceptance: find why the primitive's state is unknown to the dependency recorder and fix that layer, so a no-edit warm build installs both. Files: tools/native-unit-dependency.f or the primitive's contract (to find). Verify: OUTER-HEAD-CASE's warm diag (test/native-file-auto-e2e.f -- outer-head) shows no compile-attempt for them. Depends: lane 11 + EXT. Ownership: unassigned. Claim: unassigned.
