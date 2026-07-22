---
title: "Infer GEMV: NVFP4 reference oracle"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:41.376826+02:00"
blocks:
  - habu-infer-gemv-supported-0fa19bc9
---

Why this exists:
Kernel parity needs an independent definition of NVFP4 unpacking, scale application, accumulation, and output conversion.

Required result:
Implement a slow deterministic reference for one packed matrix-vector product using the exact recipe semantics and declared accumulation precision.

Done when:
Hand-built boundary values and random small matrices match an independent fixture; invalid scale and padding rows reject; the oracle exposes per-row results for kernel diagnostics.

Expected touch points: the reference oracle and focused host tests.
Smallest check: the focused boundary-value oracle test.
Prerequisites: supported batch-one geometry.
Owned result: correctness oracle only.
Claim: unassigned.
