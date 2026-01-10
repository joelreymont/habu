---
title: Add nth-value macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:25:20.072945+02:00"
---

Missing from stdlib. Add to lib/stdlib.habu.
Pattern: (nth-value n form) -> nth value from multiple-value form
Expand using multiple-value-bind to capture values.
Test: (nth-value 1 (values 1 2 3)) => 2
