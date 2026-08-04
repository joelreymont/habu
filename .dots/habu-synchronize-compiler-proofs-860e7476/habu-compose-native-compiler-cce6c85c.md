---
title: Compose native compiler theorem
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:04:26.822954+02:00"
blocks:
  - habu-prove-a64-obj-92f2ae05
---

Full context: compose source/checker binding, HIR, SIR, stable passes, typed heap/separation/arena ownership, native memory refinement, LIR, A64IR, allocation, encoding, HBOBJ, and loaded-image proofs for each covered native wave. Acceptance: theorem manifests bind exact schemas/versions and memory assumptions; candidate fixpoint consumes the proved chain; allocation identity, bounds, alignment, lifetime, permissions, frame preservation, and release obligations have no unbound gap; uncovered language remains named, never assumed.
