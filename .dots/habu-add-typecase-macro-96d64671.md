---
title: Add typecase macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:24:53.612196+02:00"
---

Missing from stdlib. Add to lib/stdlib.habu.
Pattern: (typecase expr (type1 body1) (type2 body2) (t default))
Expand to cond with typep checks.
Test: (typecase 42 (string 's') (number 'n') (t 'other)) => n
