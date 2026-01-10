---
title: Add multiple-value-setq macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:25:24.105957+02:00"
---

Missing from stdlib. Add to lib/stdlib.habu.
Pattern: (multiple-value-setq (var1 var2) form) -> set vars from mv form
Expand using multiple-value-bind + setq.
Test: (let (a b) (multiple-value-setq (a b) (values 1 2)) (list a b)) => (1 2)
