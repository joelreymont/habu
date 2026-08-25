---
title: Migrate remaining checked casts
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:39:28.883590+02:00"
blocks:
  - habu-cast-v2-family-741e7bae
---

Invariant: an empty same-cell conversion that changes only a checker-known
nominal role uses `CAST`, not `TRUSTED:`. A retained trusted conversion must
mint authority, change representation class, move linear ownership, or cross a
boundary the checker demonstrably cannot express.

Result: classify every syntactically empty one-cell `TRUSTED:` declaration in
the current source by semantic role and migrate every `CAST`-legal site while
preserving package confinement. Matching cell width is not evidence. Do not
convert a linear mint, representation-changing reinterpretation, or provenance
grant without checker proof. Each retained source boundary carries only its
source-local rationale, retirement owner, and focused production-path test.
Coordinate type-family behavior with `habu-cast-v2-family-741e7bae`.

Acceptance: a fresh source census contains no `CAST`-legal empty trusted
conversion and no unowned retained boundary; wrong source family, wrong
destination family, cross-package, constructor-forgery, linearity, and raw-cell
negatives reject; public effects and runtime bytes remain unchanged. Run the
CAST focused suites, every touched exact load, typed-local and package diff
gates, Maki, fixpoint, and the full native gate. Measure definitions, JIT,
DATA, CODELEN, certification time, and image identity before and after; require
a material source-trust reduction without added runtime code.
