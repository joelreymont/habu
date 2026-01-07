---
title: Build Stage 1 via FASL system
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-06T21:02:05.58267+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

Use habu:compile-file and habu:link-fasls to build Stage 1 from bootstrap modules. habu-stage1-src.lisp has full linker with native-write-file. Test with prelude.lisp + main.
