---
title: Design setf expander registry
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-08T07:20:50.481526+02:00\""
---

File: lib/stdlib.habu - Design data structure to store custom setf expanders. Hash table mapping symbols to expander functions. Plan: (defparameter *setf-expanders* (make-hash-table)). Depends on: research findings (cc13115a).
