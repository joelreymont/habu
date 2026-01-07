---
title: "Step 2: Make reg-alloc.lisp portable"
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-09T16:08:21.585744+02:00"
closed-at: "2025-12-09T19:59:44.255523+02:00"
close-reason: ""
---

Replace setf (car ...) with setcar, remove #+sbcl conditionals from core functions, make global state explicit.
