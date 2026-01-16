---
title: Add signal primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:54.054411+02:00"
---

src/runtime/primitives/primitives.zig: Implement signal
- Depends on: dot (make-condition)
- signal: invoke condition handlers without establishing restart
- Search handler chain for matching handler
- Call handler with condition
- Return if handler returns normally
- Add tests for handler invocation
- Est: 25 min
