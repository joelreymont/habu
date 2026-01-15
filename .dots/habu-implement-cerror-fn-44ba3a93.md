---
title: Implement cerror function
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:38:02.269807+02:00"
---

stdlib.habu: Add (defun cerror (continue-string error-string &rest args) ...). Use restart-case to establish continue restart, then signal error. Pattern: (restart-case (error error-string) (continue () nil)). Dependencies: habu-add-handler-bind-d839f9af. Verify: (cerror "Continue" "Test error")
