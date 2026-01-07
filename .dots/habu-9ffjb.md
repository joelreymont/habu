---
title: "Milestone 1: Fix h0-eval-ir closure bugs"
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-09T14:30:20.578652+02:00"
closed-at: "2025-12-09T14:43:20.424267+02:00"
close-reason: ""
---

Get compiled closures working so we can verify codegen correctness.

Success criteria:
- `256 (let ((n 5)) (funcall (lambda (x) (+ x n)) 10))` returns 15
- `256 (let ((a 1) (b 2)) (funcall (lambda (x) (+ x (+ a b))) 10))` returns 13
- All 130 bootstrap tests pass
