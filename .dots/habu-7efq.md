---
title: Comprehensive IR pattern propagation review
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-07T21:21:19.738505+02:00"
closed-at: "2025-12-08T20:16:12.159583+02:00"
close-reason: ""
---

Review all IR patterns in bootstrap compiler to ensure they are properly linearized (ir-to-tac) and codegenned (tac-codegen). Focus on null check path: cmp-eq, nil-ir, if-ir, and how they connect. Find why null check fails at runtime.
