---
title: Prove stable SIR passes
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:04:26.754673+02:00"
blocks:
  - habu-canonicalize-typed-native-7d698b51
  - habu-prove-hir-to-1be23c02
---

Full context: prove stable SIR transformations used by covered native slices, including exact integer folding, dead pure elimination, CFG simplification, and retained deterministic structural rewrites. Consume `Common/Separation.v`: every removal, forwarding, or reordering uses declared footprints, sound alias/effect classes, locality, and the frame theorem; volatile, atomic, fence, and unknown effects are never inferred pure. Acceptance: explicit overflow/numeric/memory premises, per-pass theorems/vectors, corrupted footprint/alias witness rejection, and assumptions report pass with no Admitted.
