---
title: Seal CAD-EFFECT authority surface
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:26:04.242036+02:00"
blocks:
  - habu-checker-sealed-destructure-d967fc03
---

Full context: the finite effect vocabulary must be assembled in a reopenable package, but private constructors, arena handles, and validation state are not an authority until the final package and its immutable storage interface are permanently sealed. Fix: add a dedicated src/cad/effect-authority.f assembly owner plus hostile seal tests; load the complete row algebra, admit only handles validated by the sealed immutable nominal arena owner, expose no raw handle mint/cast/storage escape, validate the public surface, and seal CAD-EFFECT exactly once after all constituents. Acceptance: public row construction, canonical UNION, validation, and legality tables remain callable; hostile reopen, qualified definition/publication, export, tick, postpone, undefine, private constructor lookup, raw-handle fabrication, cross-owner handle substitution, and mutation of frozen arena storage reject; snapshot, AOT, rollback, replay, and fixpoint preserve both the package seal and immutable arena authority. Files: src/cad/effect-authority.f and its focused seal test. Any surviving source TRUST keeps only its source-local rationale, retirement owner, and focused production-path test. Verify: effect, nominal-forgery, protected-span, seal-hostile, public-signature, bootstrap, fixpoint, and full native gates. Depends on the finite row vocabulary, immutable nominal arena, and sealed-package syntax.

Design-reference (2026-07-18, tfinite): implements `docs/effects.md` § R8-1 (the sealed authority boundary). Acceptance: `src/cad/effect-authority.f` loads the complete row algebra, admits only handles validated by the sealed immutable nominal arena, exposes no raw handle mint/cast/storage escape, and seals `CAD-EFFECT` exactly once after all constituents; hostile reopen, qualified redefinition/publication, export, tick, postpone, undefine, private-constructor lookup, raw-handle fabrication, cross-owner handle substitution, and frozen-arena mutation reject, while snapshot/AOT/rollback/replay/fixpoint preserve both the package seal and the immutable arena authority — mutation-matrix rows 1-4 and 15.
