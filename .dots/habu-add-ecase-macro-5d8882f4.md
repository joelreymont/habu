---
title: Add ecase macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:24:57.646187+02:00"
---

Missing from stdlib. Add to lib/stdlib.habu.
Pattern: (ecase keyform (key1 body1) (key2 body2))
Like case but signals error if no match. No default clause.
Test: (ecase 'a (a 1) (b 2)) => 1
