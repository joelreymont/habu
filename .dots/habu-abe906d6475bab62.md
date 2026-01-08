---
title: Implement basic setf for simple places
status: open
priority: 2
issue-type: task
created-at: "2026-01-08T06:25:26.036895+02:00"
---

File: lib/stdlib.habu - First phase of setf: handle simple built-in places without expansions. (setf (car x) val) → (rplaca x val), (setf (cdr x) val) → (rplacd x val), (setf (symbol-value sym) val) → (set sym val), (setf (aref arr ...) val) → (%aset arr ... val). Use pattern matching on place form. Depends on: none (can start immediately).
