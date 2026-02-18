---
title: Large object space
status: open
priority: 1
issue-type: task
created-at: "2026-02-18T21:50:53.715891+01:00"
blocks:
  - habu-minor-gc-collector-2f89a428
---

src/runtime/heap.zig allocVector/allocBaseString/allocChunk. Cause: copying large objects dominates pause time and cache bandwidth. Fix: allocate large payloads in LOS with mark-sweep and pin semantics. Why: avoid repeated megabyte copies.
