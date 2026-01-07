---
title: Fix read-all-loop spill offset bug in habu0 reg-alloc
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-07T21:34:58.901844+02:00"
closed-at: "2025-12-08T14:07:41.954479+02:00"
close-reason: ""
---

Investigate spill slot offset mismatch in READ-ALL-LOOP (reverse acc) causing nil spill/load mixup in compiled binary.
