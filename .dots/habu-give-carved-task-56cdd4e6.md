---
title: Give carved task-local rows a record type
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T12:55:57.007232+01:00"
---

Problem: modules carve a TASK:+USERS byte row into fields by offsets, though TASK:+USER answers a typed ( -- ^t ) accessor: src/lib/fd.f:134-167 (POLL-STORAGE: a struct pollfd by LE offsets 0, 4 and 6), src/lib/net/udp4.f:58-60 (IO-STORAGE: a sockaddr at 0 and its length at $10), src/lib/net/datagram.f:96-98 (OPTION: a getsockopt value at 0 and its length at 4), src/lib/net/curl.f:185-189 (IO-STORAGE: four result cells at 0, 8, $10 and $18). Acceptance: each such row is a STRUCTURE held through TASK:+USER(S), or one row per role (a byte row stays only where an OS call fills raw bytes and says so); no offset sum or index carves a task-local row in src/lib; the modules' tests pass. Depends: none. Claim: unassigned.
