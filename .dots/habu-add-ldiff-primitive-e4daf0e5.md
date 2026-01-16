---
title: Add ldiff primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:24.633912+02:00"
---

src/runtime/primitives/list.zig: Implement ldiff function
- ldiff: copy list up to specific tail
- Return copy of list elements before tail starts
- Handle case where tail not found (copy entire list)
- Non-destructive operation
- Add tests for various tail positions
- Est: 15 min
