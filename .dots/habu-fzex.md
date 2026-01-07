---
title: Investigate habu0 nil check crash at MAIN+620
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-07T21:13:36.801035+02:00"
closed-at: "2025-12-09T13:14:27.924311+02:00"
close-reason: ""
---

Debug habu0 crash at MAIN+620 (0x29304) where CAR on nil despite null check. Review reg-alloc.lisp, codegen.lisp, habu0.lisp segments.
