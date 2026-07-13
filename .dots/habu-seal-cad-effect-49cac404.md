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

Full context: the finite effect vocabulary must be assembled in a reopenable package, but private constructors, arena handles, and validation state are not an authority until the final package and its immutable storage interface are permanently sealed. Fix: add a dedicated src/cad/effect-authority.f assembly owner plus hostile seal tests; load the complete row algebra, admit only handles validated by the sealed immutable nominal arena owner, expose no raw handle mint/cast/storage escape, validate the public surface, and seal CAD-EFFECT exactly once after all constituents. Acceptance: public row construction, canonical UNION, validation, and legality tables remain callable; hostile reopen, qualified definition/publication, export, tick, postpone, undefine, private constructor lookup, raw-handle fabrication, cross-owner handle substitution, and mutation of frozen arena storage reject; snapshot, AOT, rollback, replay, and fixpoint preserve both the package seal and immutable arena authority. Files: src/cad/effect-authority.f, focused seal test, TRUSTED.md and FILEMAP.md only as required. Verify: effect, nominal-forgery, protected-span, and seal hostile suites; trust/refine/public-signature lints; bootstrap/fixpoint/full native gates. Depends on the finite row vocabulary, immutable nominal arena, and sealed-package syntax.
