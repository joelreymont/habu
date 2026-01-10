---
title: Add substitute function
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:25:07.038463+02:00"
---

Missing from stdlib. Add to lib/stdlib.habu.
Pattern: (substitute new old seq) -> seq with old replaced by new
For lists: (if (eql (car seq) old) (cons new ...) (cons (car seq) ...))
Test: (substitute 'x 2 '(1 2 3 2 4)) => (1 x 3 x 4)
