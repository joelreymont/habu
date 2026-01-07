---
title: Fix macro-in-recursive test (recursion + macros)
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-07T08:10:48.714173+02:00"
closed-at: "2025-12-07T15:39:15.549136+02:00"
close-reason: ""
---

macro-in-recursive test returns 0 instead of 42. Macros inside recursive functions may have evaluation order or binding issues.
