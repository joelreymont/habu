---
title: "Nominal storage: typed definers"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T16:08:46.264849+02:00"
blocks:
  - habu-nominal-storage-effect-a60ba885
---

Problem: LAYOUT-BUFFER rejects arity-zero families and no sound typed variable/buffer definer exists. Acceptance: generalize storage validation for closed non-linear addressable arity-zero TYPEFAMILY, nominal atom, closed layout family and closed typed pointer; add uppercase TYPED-VARIABLE and TYPED-BUFFER with checked extent, overflow, zeroing, typed accessor effects, transactional allocation and definition rollback, snapshot/fixpoint/bootstrap parity; reject open variables, quotations, linear values, hidden fields, unresolved args and duplicates. No unconstrained typed create. Typed constants require a checked producer. Files: src/core/checker.f, storage definer owner, roles.f if needed, native/bootstrap definer grammar, engine/storage tests, docs/type-families.md. Verify: same-family storage positives, cross-family/bounds/overflow/rollback negatives, native fixpoint/bootstrap/full gates. Depends: habu-nominal-storage-effect-a60ba885.
