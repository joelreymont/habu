---
title: Fix reg-alloc.lisp native path CL dependencies
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-14T08:03:57.835022+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

Native #-sbcl path uses dolist (lines 3909, 3936), setf (675, 676), append in loops (4061, 4199), string= (line 310). Replace with habu-compatible code.
