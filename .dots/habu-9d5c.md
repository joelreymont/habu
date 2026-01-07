---
title: Remove duplicate codegen paths - use only linear codegen with register allocator
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-07T16:17:55.761613+02:00"
closed-at: "2025-12-08T14:27:32.919716+02:00"
close-reason: ""
---

Register clobbering bug caused by using wrong codegen. Must consolidate to single linear codegen with proper register allocation. Remove *use-register-allocation* conditional and accumulator-based codegen.
