---
title: Test handler-bind integration
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:43:11.442622+02:00"
---

Create test: (handler-bind ((error (lambda (c) (format t "caught: ~A" c)))) (error "test")). Verify handler invoked. Dependencies: habu-add-handler-bind-d839f9af. Verify: handler catches error.
