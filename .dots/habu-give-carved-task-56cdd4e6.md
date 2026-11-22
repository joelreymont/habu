---
title: Give carved task-local rows a record type
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T12:55:57.007232+01:00"
---

Problem: modules carve a TASK:+USERS byte row into fields by offsets, though TASK:+USER answers a typed ( -- ^t ) accessor: src/lib/net/curl.f:185-189 (IO-STORAGE: four result cells at 0, 8, $10 and $18); src/lib/net/httpc.f:165,557-560 carves its conn's BUFFER scratch 8 the same way (a getsockopt value at 0, its length at 4). Acceptance: each such row is a STRUCTURE held through TASK:+USER(S), or one row per role (a byte row stays only where an OS call fills raw bytes and says so); no offset sum or index carves a task-local row in src/lib; the modules' tests pass. Depends: none. Claim: unassigned.
