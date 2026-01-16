---
title: Add sequence fill primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:20.694954+02:00"
---

src/runtime/primitives/vector.zig or list.zig: Implement fill function
- fill: set all/range of sequence elements to value
- Support :start/:end keyword parameters
- Handle both lists and vectors
- Destructive operation (modify in place)
- Add tests for partial fills
- Est: 20 min
