---
title: Add stable-sort primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:22.209178+02:00"
---

src/runtime/primitives/vector.zig: Implement stable sort
- stable-sort: merge sort preserving element order for equal keys
- Support :key parameter for custom key extraction
- Handle both destructive and non-destructive modes
- Work on both lists and vectors
- Add tests verifying stability
- Est: 30 min
