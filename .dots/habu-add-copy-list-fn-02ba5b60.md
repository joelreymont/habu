---
title: Add copy-list function
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:24:41.580773+02:00"
---

Missing from stdlib. Add to lib/stdlib.habu.
Pattern: (copy-list lst) -> new list with same elements (shallow copy)
Simple recursive: (if (null lst) nil (cons (car lst) (copy-list (cdr lst))))
Test: (copy-list '(1 2 3)) => (1 2 3)
