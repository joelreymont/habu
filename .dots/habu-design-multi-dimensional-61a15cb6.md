---
title: Design multi-dimensional array type
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T14:03:46.337441+02:00"
---

src/runtime/objects.zig: Design Array object type separate from Vector. Store rank, dimensions array, and flat data. Vector remains 1D specialized type. Array handles 2D+. Row-major layout. Add Array tag to Value (reuse unused tag bits or extend scheme). Est: 45 min
