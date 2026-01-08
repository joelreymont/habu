---
title: Implement get-setf-expansion
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-08T07:20:56.226212+02:00\""
---

File: lib/stdlib.habu - Implement function to retrieve setf expansion for a place. Check *setf-expanders* registry, fall back to built-in expansions. Returns: vars, vals, store-vars, writer, reader. Depends on: registry design (57a372ef).
