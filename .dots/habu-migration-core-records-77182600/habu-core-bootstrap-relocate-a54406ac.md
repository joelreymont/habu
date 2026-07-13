---
title: "Core bootstrap: relocate CELL"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-13T18:00:18.063041+02:00\\\"\""
closed-at: "2026-07-13T19:13:53.101518+02:00"
close-reason: Relocated CELL and its effects into dedicated pre-checker owners with native/recovery/fixpoint/pin/cache parity at 04eedf53; destruction review passed and full exact-tree gate was green before master fast-forward.
---

Move CELL out of src/core/structures.f into the earliest architecture constant owner or a dedicated one-concern source loaded before every user. Preserve the target cell-byte invariant in native and recovery sources with load-time equality assertions and focused bootstrap/fixpoint parity tests. Do not retain any legacy structure dependency.

Claim: agent=cell_boot workspace=.jj-ws/type-dsl-cell
