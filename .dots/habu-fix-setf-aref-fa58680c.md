---
title: Fix setf aref for arrays
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T07:18:10.050761+02:00"
---

src/compiler/compile.zig - setf expansion

(setf (aref a 0 1) 42) => type mismatch error

Depends on: 4c72fda5 (aref must work first)

Check how setf expands aref - should use %aset.

Test:
(let ((a (make-array '(2 3) :initial-element 0)))
  (setf (aref a 1 1) 99)
  (aref a 1 1)) => 99
