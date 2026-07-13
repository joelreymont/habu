---
title: "Core records: remove checker frames"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:15:59.125233+02:00"
blocks:
  - habu-core-records-remove-07fdf718
---

Own remaining checker control, MATCH, locals-width, and lowering frame layouts
in src/core/checker.f plus focused engine tests. Replace raw structure definers
with named cell/byte offsets, named strides, ordinary accessors, and load-time
offset, size, alignment, and pointer-role assertions; preserve stack-state,
snapshot, and diagnostic semantics exactly.
