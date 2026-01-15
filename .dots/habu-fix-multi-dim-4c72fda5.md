---
title: Fix multi-dim array aref
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T07:18:02.752517+02:00"
---

src/runtime/primitives/array.zig or vector.zig

(defvar a (make-array '(2 3) :initial-element 0))
(aref a 0 1) => :initial-element (WRONG, should be 0)

The array is being created but aref doesn't index correctly.
Check how multi-dim indices are computed.

Test:
(aref (make-array '(2 3) :initial-element 5) 1 2) => 5
