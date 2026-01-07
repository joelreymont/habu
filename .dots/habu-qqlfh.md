---
title: Implement find-unguarded MCP tool for nil/type check analysis
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-10T14:05:24.987722+02:00"
closed-at: "2025-12-10T14:12:33.488468+02:00"
close-reason: ""
---

Create MCP tool that lists dereferences without nil/type guards. Would flag places like `ldr x3, [x1]` where x1 could be nil/wrong-type without prior check.
