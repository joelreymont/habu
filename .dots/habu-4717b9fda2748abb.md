---
title: ignore-errors macro
status: closed
priority: 1
issue-type: task
created-at: "2025-12-29T16:04:50.491006+02:00"
closed-at: "2025-12-29T17:52:35.391339+02:00"
close-reason: "Already exists in stdlib.habu:55"
---

Wrap handler-case to return nil on any error.
Location: stdlib.habu
Syntax: (ignore-errors form*) => primary-value, condition
Implementation:
  (defmacro ignore-errors (&rest forms)
    `(handler-case (progn ,@forms)
       (error (c) (values nil c))))
Depends on: handler-case (DONE), multiple-value support
