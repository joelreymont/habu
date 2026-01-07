---
title: "Bug: h0-eval defun with if returns wrong value"
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-06T07:37:48.623786+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

(defun test (x) (if (null x) 100 200)) (test nil) returns 210, should be 100. Simple defuns work: (defun f (x) (+ x 1)) (f 5) returns 6. Issue appears when if is in function body.
