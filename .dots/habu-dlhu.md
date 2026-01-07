---
title: Review bootstrap labels/closure lifting bug for habu-read LAMBDA-9
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-08T04:21:44.833501+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

Investigate labels-generated LAMBDA-9 receiving 2-element list but CADDR is accessed in habu0.lisp lines 422-482; review bootstrap/compiler-sbcl.lisp and bootstrap/codegen.lisp focusing on labels lifting and closure env.
