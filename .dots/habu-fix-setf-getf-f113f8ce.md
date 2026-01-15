---
title: Fix setf getf expansion
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T07:18:33.281030+02:00"
---

stdlib.habu or compile.zig

(let ((x '(:a 1))) (setf (getf x :a) 10) x) => CompileError

setf needs to handle getf places. May need define-setf-expander
or special case in setf macro.

Test:
(let ((plist '(:a 1 :b 2)))
  (setf (getf plist :a) 99)
  (getf plist :a)) => 99
