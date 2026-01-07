---
title: cerror function
status: closed
priority: 2
issue-type: task
created-at: "\"2025-12-29T16:05:59.203554+02:00\""
closed-at: "\"2025-12-29T19:09:55.905612+02:00\""
blocks:
  - habu-4717bca461c2e76f
---

Implement cerror for continuable errors.
Location: stdlib.habu
Syntax: (cerror continue-format-string error-format-string &rest args)
Examples:
  (cerror "Skip this item" "Invalid item: ~A" item)
  ;; Signals error with CONTINUE restart available
Implementation:
  (defun cerror (continue-string error-string &rest args)
    (restart-case (apply #'error error-string args)
      (continue () :report (lambda (s) (format s continue-string)) nil)))
Blocked by: restart-case
