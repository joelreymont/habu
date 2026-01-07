---
title: Require keyword register names in ARM64 assembler
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-04T12:33:47.254489+02:00"
closed-at: "2025-12-04T13:44:48.394945+02:00"
close-reason: ""
---

Modify arm64:reg function to reject raw numbers and only accept keyword symbols like :x0, :x1, :sp, :env etc. This enforces readable code and catches errors at compile time.
