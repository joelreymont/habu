---
title: Seal CAD-EFFECT authority surface
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:26:04.242036+02:00"
blocks:
  - habu-define-finite-cad-0bdf52ad
  - habu-seal-owners-syntax-63051652
---

Full context: the finite effect vocabulary must be assembled in a reopenable package, but private constructors and validation state are not an authority until the final package is permanently sealed. Fix: add a dedicated src/cad/effect-authority.f assembly owner plus hostile seal tests; load the complete row algebra, validate its public surface, and seal CAD-EFFECT exactly once after all constituents. Acceptance: public row construction, canonical UNION, validation, and legality tables remain callable; hostile reopen, qualified definition/publication, export, tick, postpone, undefine, and private constructor lookup reject; snapshot, AOT, rollback, and fixpoint preserve the seal. Files: src/cad/effect-authority.f, focused seal test, TRUSTED.md and FILEMAP.md only as required. Verify: effect and seal hostile suites, trust/refine/public-signature lints, bootstrap/fixpoint/full native gates. Depends on the finite row vocabulary and sealed-package syntax.
