---
title: Add tests for undefined function detection
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-05T13:47:15.956761+02:00"
closed-at: "2025-12-05T13:51:11.269494+02:00"
close-reason: ""
---

Add tests to verify that undefined functions are caught at compile-time. Tests should verify: 1) undefined functions are recorded, 2) compilation aborts with clear error, 3) valid code still compiles
