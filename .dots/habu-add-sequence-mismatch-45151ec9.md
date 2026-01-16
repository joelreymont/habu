---
title: Add sequence mismatch primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:21.601468+02:00"
---

src/runtime/primitives/vector.zig: Implement mismatch function
- mismatch: find first position where sequences differ
- Support :start1/:end1/:start2/:end2/:from-end/:test/:key
- Handle both list and vector sequences
- Return position or nil if equal
- Add tests for prefix/suffix mismatches
- Est: 25 min
