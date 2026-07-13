---
title: "Core bootstrap: relocate CELL"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T18:00:18.063041+02:00"
blocks:
  - habu-type-dsl-specify-db2bf883
---

Move CELL out of src/core/structures.f into the earliest architecture constant owner or a dedicated one-concern source loaded before every user. Preserve the target cell-byte invariant in native and recovery sources with load-time equality assertions and focused bootstrap/fixpoint parity tests. Do not retain any legacy structure dependency.
