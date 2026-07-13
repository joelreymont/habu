---
title: "Libraries: migrate runtime records"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:17:07.394618+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own raw structure declarations in lib/vector.f and lib/task.f plus focused tests. Convert them to typed STRUCTURE with exact field schemas, byte offsets, alignment, generated accessors, and MAKE/UNMAKE where used. Preserve ABI and hot-path allocation behavior.
