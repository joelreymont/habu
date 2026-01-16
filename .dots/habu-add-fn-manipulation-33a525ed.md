---
title: Add function manipulation primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:32.204391+02:00"
---

src/runtime/primitives/primitives.zig: Implement function utilities
- constantly: return function that always returns given value
- complement: return function that negates predicate
- Both return closures over given function/value
- Add tests for returned function behavior
- Est: 15 min
