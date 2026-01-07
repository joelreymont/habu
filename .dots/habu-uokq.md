---
title: Integrate register allocator into codegen pipeline
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-06T22:53:34.79123+02:00"
closed-at: "2025-12-06T22:54:48.767337+02:00"
close-reason: ""
---

Replace naive 24-slot temp allocation with proper register allocator. The reg-alloc.lisp already has full pipeline: ir-to-tac, liveness analysis, linear-scan allocation. Just needs to be wired in.
