---
title: Synchronize compiler proofs
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:53:09.605242+02:00"
---

Full context: design sections 10-12 and 16.6 require executable/Rocq schema parity, validator proofs, witness vectors, assumptions reports, composed native/GPU theorems, and bootstrap linkage. Required result: each stable implementation schema has a digest-matched proof owner and corrupted-witness negatives. Acceptance: no Admitted; expected external axioms only; final proof chains reach AArch64 loaded-image and PTX semantics for covered slices.
