---
title: Migrate remaining checked casts
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:39:28.883590+02:00"
blocks:
  - habu-cast-v2-family-741e7bae
---

Invariant: an empty same-cell conversion that only changes a checker-known nominal role uses CAST, not TRUSTED; TRUSTED remains only where the operation mints authority, changes representation class, moves linear ownership, or crosses another proved unexpressible boundary. The checked CAST primitive is landed and the refinement-lint coupling blocker is closed, yet the frozen Maki census still contains 68 eligible empty TRUSTED converters. A repository-wide syntax census finds 161 empty one-cell TRUSTED declarations that still require semantic classification.

Convert all 68 already-proved Maki sites to CAST, remove their trust inventory rows, and preserve package confinement. Then classify every remaining syntactically empty one-cell TRUSTED declaration by semantic role. Migrate every CAST-legal site; for each retained boundary, record the exact invariant CAST cannot express and assign it to an existing capability owner or create one precise leaf. Do not treat matching cell width as sufficient evidence, and do not convert linear mints, representation-changing class reinterpretations, or provenance grants without checker proof. Coordinate family ownership with habu-cast-v2-family-741e7bae.

Prove the frozen 68-site census reaches zero, the repository classification has no unowned residual, wrong source family, wrong destination family, cross-package, constructor-forgery, linearity, and raw-cell negatives still reject, and public effects and runtime bytes remain unchanged. Verify CAST suites, refinement lint, trust inventory and ratchet, typed-local and package gates, every touched exact load, Maki, fixpoint, and full native gates. Measure TRUSTED rows, definitions, JIT, DATA, CODELEN, certification time, and image identity before and after; require a material trust reduction without added runtime code.
