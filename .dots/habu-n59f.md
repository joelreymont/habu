---
title: Implement debug info as nanopass (extract-debug-vars + emit-debug-table)
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-08T13:39:20.736204+02:00"
closed-at: "2025-12-08T14:09:41.370172+02:00"
close-reason: ""
---

Implement debug info extraction as two nanopasses: 1) extract-debug-vars walks IR to extract variable names and slots before linearization, 2) emit-debug-table combines with fnoffs to emit binary table.
