---
title: Large object space
status: active
priority: 1
issue-type: task
created-at: "\"2026-02-18T21:50:53.715891+01:00\""
blocks:
  - habu-minor-gc-collector-2f89a428
---

src/runtime/heap.zig + src/runtime/gc.zig. Cause: large allocations still flowed through nursery/tenured copy paths and lacked pinned-space reclamation. Fix: allocate large payloads in LOS (threshold-based), track LOS metadata, mark+scan LOS roots, and sweep/reuse LOS free spans non-movingly. Why: avoid repeated large copies while keeping memory bounded.
