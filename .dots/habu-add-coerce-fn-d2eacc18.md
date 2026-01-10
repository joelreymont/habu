---
title: Add coerce function
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:24:49.159228+02:00"
---

Missing from stdlib. Add to lib/stdlib.habu.
Pattern: (coerce obj type) -> convert obj to type
Support: list->vector, vector->list, string->list, list->string
Test: (coerce '(1 2 3) 'vector) => #(1 2 3)
