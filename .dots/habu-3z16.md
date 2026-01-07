---
title: Integrate register allocator into SBCL compilation pipeline
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-05T09:28:12.334448+02:00"
closed-at: "2025-12-05T12:42:25.922478+02:00"
close-reason: ""
---

Register allocator exists in reg-alloc.lisp (TAC pipeline with linear-scan) but is not used by default. Need to wire it into compiler-sbcl.lisp codegen path. This eliminates the env/spill slot collision bug and produces better code. Required for cross-platform bootstrap.
