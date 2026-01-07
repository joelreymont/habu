---
title: Fix Stage 1 interpreter symbol dispatch
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-05T21:12:36.957413+02:00"
closed-at: "2025-12-06T21:32:04.079529+02:00"
close-reason: ""
---

Stage 1 (habu0) builds successfully but operator dispatch fails. Numbers work (42 returns 42), progn works, if works, but arithmetic operators like + are not recognized. The issue is in symbol comparison - op=plus uses h0-string= to compare symbol-name results but the dispatch falls through. Need to debug symbol-name implementation and string comparison in native code.
