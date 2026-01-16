---
title: Fix property test error handling
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T12:08:27.465613+02:00"
---

src/tests/property.zig:60 - catch unreachable in tests, rule violation. Propagate errors. Low severity.
