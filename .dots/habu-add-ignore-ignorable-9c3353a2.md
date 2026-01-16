---
title: Add ignore/ignorable declaration handling
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:42.845611+02:00"
---

src/compiler/compile.zig: Implement ignore warnings
- Depends on: dot (declare special form)
- Track which variables are declared ignore/ignorable
- Suppress unused variable warnings for ignored vars
- Warn if ignored var is actually used
- Add tests for ignore declarations
- Est: 20 min
