---
title: Test cerror integration
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:43:16.592179+02:00"
---

Create test: (handler-bind ((error (lambda (c) (invoke-restart 'continue)))) (cerror "Continue" "Test error")). Verify restart works. Dependencies: habu-implement-cerror-fn-44ba3a93. Verify: cerror + continue works.
