---
title: Give carved task-local rows a record type
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T12:55:57.007232+01:00"
blocks:
  - habu-take-curl-s-ed37df96
---

Problem: modules carve a TASK:+USERS byte row into fields by offsets, though TASK:+USER answers a typed ( -- ^t ) accessor: src/lib/net/curl.f:185-189 (IO-STORAGE: four result cells at 0, 8, $10 and $18). Acceptance: each such row is a STRUCTURE held through TASK:+USER(S), or one row per role (a byte row stays only where an OS call fills raw bytes and says so); no offset sum or index carves a task-local row in src/lib; the modules' tests pass. Depends: habu-take-curl-s-ed37df96 (carl's lane edits curl.f). Claim: unassigned.
