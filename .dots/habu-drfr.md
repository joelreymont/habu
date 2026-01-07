---
title: Add property tests for match expansion
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-05T15:16:40.055703+02:00"
closed-at: "2025-12-05T15:23:38.70947+02:00"
close-reason: ""
---

Add QuickCheck property tests for match macro expansion. Test the source transformation, not execution (fast). Properties: binding identity, wildcard exhaustiveness, cons reconstruction, first-match semantics.
