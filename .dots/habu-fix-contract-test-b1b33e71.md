---
title: Fix contract test error handling
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T12:08:34.657096+02:00"
---

src/types/contract.zig:535 - catch unreachable in tests, rule violation. Use try/expectError. Low severity.
