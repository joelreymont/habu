---
title: Add merge sequence primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:22.513898+02:00"
---

src/runtime/primitives/vector.zig: Implement merge function
- merge: merge two sorted sequences into one
- Support :key parameter for custom key extraction
- Result type specified as first argument
- Handle both lists and vectors
- Add tests for various sequence types
- Est: 25 min
