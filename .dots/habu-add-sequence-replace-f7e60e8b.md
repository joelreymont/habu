---
title: Add sequence replace primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:20.998712+02:00"
---

src/runtime/primitives/vector.zig: Implement replace function
- replace: copy elements from one sequence to another
- Support :start1/:end1/:start2/:end2 parameters
- Handle both list and vector sources/destinations
- Destructive on destination sequence
- Add tests for overlapping ranges
- Est: 25 min
