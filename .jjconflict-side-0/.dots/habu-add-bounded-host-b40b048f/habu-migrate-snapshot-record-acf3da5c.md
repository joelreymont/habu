---
title: Migrate snapshot record to frozen MEM bytes
status: closed
priority: 2
issue-type: task
created-at: "2026-07-13T16:45:22.114815+02:00"
closed-at: "2026-08-02T16:00:35.790010+02:00"
close-reason: "Invalid and underspecified: the task names no record, path, owner, or production defect, so it has no implementable scope."
blocks:
  - habu-freeze-transient-mem-1a69322a
---

Problem: one snapshot record path can persist raw pointer/length provenance without proving immutable owned bytes or schema/content identity. Fix: migrate one complete snapshot record to MEM FREEZE output and reconstruct a fresh owner on load; never persist region/generation/address authority. Acceptance: transient pointer serialization, wrong schema, digest mismatch, truncated bytes, and replay into the wrong record kind reject; valid snapshot round-trip is byte-stable and independent of allocation address/order. Files: one snapshot owner module plus focused test and TRUSTED.md only if a row is discharged. Verify: snapshot/rebase/fixpoint tests, mutation and replay negatives, trust/typed-local lints, full native gate.
