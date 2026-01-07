---
title: Integrate hash tables with compiler
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-04T22:11:16.281639+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

Runtime hash-tables.lisp exists but no compiler IR. Need: make-hash-table, gethash, (setf gethash), remhash, maphash, hash-table-count. Currently uses malloc, should integrate with GC.
