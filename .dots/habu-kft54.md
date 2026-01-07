---
title: Fix h0-compile-add to handle variadic arguments
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-09T13:16:39.836608+02:00"
closed-at: "2025-12-09T13:28:17.118675+02:00"
close-reason: ""
---

h0-compile-add only handles 2 args, ignoring rest. (+ 1 2 100) returns 3 not 103. Need to recurse for multi-arg addition.
