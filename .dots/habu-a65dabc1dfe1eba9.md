---
title: Add setf support for gethash
status: open
priority: 2
issue-type: task
created-at: "2026-01-08T06:25:40.337443+02:00"
---

File: lib/stdlib.habu - Extend setf for hash tables: (setf (gethash key ht) val) → (puthash key ht val). Simple expansion since puthash primitive already exists. Depends on: basic setf implementation.
