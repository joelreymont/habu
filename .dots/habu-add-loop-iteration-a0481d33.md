---
title: Add LOOP iteration destructuring
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:51.634101+02:00"
---

lib/stdlib.habu: Support destructuring in for clauses
- Depends on: dot (LOOP design)
- for (a b) in list: destructure list elements
- for (key . value) being hash-keys...: destructure pairs
- Use destructuring-bind internally
- Add tests for destructured iteration
- Est: 25 min
