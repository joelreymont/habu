---
title: Implement get-setf-expansion function
status: open
priority: 2
issue-type: task
created-at: "2026-01-08T06:24:41.087805+02:00"
---

File: lib/stdlib.habu - Runtime function that returns 5 values for setf expansion: (vars vals store-vars writer-form reader-form). Used by setf macro to handle complex places. Standard CL interface for extensible places. Depends on: define-setf-expander being implemented.
