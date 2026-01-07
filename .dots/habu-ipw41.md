---
title: Add dolist/when/unless macro expansion to habu0
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-09T19:01:41.475781+02:00"
closed-at: "2025-12-10T21:42:37.022048+02:00"
close-reason: ""
---

ir-to-tac fails on DOLIST-IR because habu0 doesn't expand these macros. Need to add macro expansion in h0-compile so dolist/when/unless expand to primitives that ir-to-tac already handles.
