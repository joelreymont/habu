---
title: Fix ignore-errors macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T07:18:21.297408+02:00"
---

stdlib.habu:55

(ignore-errors (/ 1 0)) => type mismatch (should return nil)

ignore-errors should catch any error and return (values nil condition).
Currently failing with type mismatch.

Depends on: 071cfeb4 (handler-case must work)

Test:
(ignore-errors (error "boom")) => nil
