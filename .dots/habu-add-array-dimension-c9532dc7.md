---
title: Add array dimension primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:18.857657+02:00"
---

src/runtime/primitives/vector.zig: Implement array dimension queries
- array-dimension: get size of specific dimension
- array-dimensions: get list of all dimension sizes
- array-total-size: total number of elements
- array-rank: number of dimensions
- Handle both vectors and multi-dimensional arrays
- Add tests for 1D, 2D, 3D arrays
- Est: 20 min
