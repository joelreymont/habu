---
title: Add &rest and &optional to lambda
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-04T22:11:20.288674+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

Lambda currently only supports fixed args. Need &rest for varargs, &optional for optional args (defun has these but lambda doesn't). Required for many CL patterns.
