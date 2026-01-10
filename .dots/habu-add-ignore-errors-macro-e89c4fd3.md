---
title: Add ignore-errors macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:24:37.344440+02:00"
---

Missing from stdlib. Add to lib/stdlib.habu.
Pattern: (ignore-errors form) -> handler-case that catches all errors and returns nil
Depends on: working handler-case or catch/throw
Test: (ignore-errors (/ 1 0)) => nil
