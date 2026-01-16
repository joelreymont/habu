---
title: Add sequence search primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:21.299639+02:00"
---

src/runtime/primitives/vector.zig: Implement search function
- search: find subsequence within sequence
- Support :start/:end/:from-end/:test/:key parameters
- Handle both list and vector sequences
- Return starting position or nil
- Add tests for various patterns
- Est: 30 min
