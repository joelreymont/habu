---
title: Synchronize compiler proofs
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:53:09.605242+02:00"
blocks:
  - habu-enforce-compiler-proof-d09bd49a
---

Full context: design sections 10-12 and 16.6 require executable/Rocq schema parity, validator proofs, witness vectors, assumptions reports, composed native/GPU theorems, and bootstrap linkage. Each stable schema proof leaf may start when its provider lands; this parent does not impose a sealed-facade wait. Common memory, separation, and arena leaves own typed heap semantics, locality/frame and alias/effect laws, and compiler storage lifecycle ownership before downstream native/GPU refinements consume them. Required result: each stable implementation schema has a digest-matched proof owner and corrupted-witness negatives. Acceptance: no Admitted; expected external axioms only; final proof chains reach race-free PTX semantics and AArch64 loaded-image semantics for covered slices.
