---
title: Investigate BUFFER-TO-STRING register allocator failure
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-07T17:51:20.083965+02:00"
closed-at: "2025-12-09T12:08:58.438905+02:00"
close-reason: ""
---

Trace register allocation pipeline for BUFFER-TO-STRING error: codegen-fn-reg-alloc returning nil though ir-to-tac looks correct. Identify failing stage (tac handlers, liveness, linear scan, null checks) and propose fix.
