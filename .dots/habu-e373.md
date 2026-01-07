---
title: Remove accumulator codegen - keep only register-allocated linear codegen
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-07T16:19:31.800197+02:00"
closed-at: "2025-12-08T14:27:33.034012+02:00"
close-reason: ""
---

Remove the buggy accumulator-based codegen entirely. Keep only the register-allocated linear codegen in reg-alloc.lisp. Remove *use-register-allocation* toggle. Update AGENTS.md to remove obsolete annotations.
