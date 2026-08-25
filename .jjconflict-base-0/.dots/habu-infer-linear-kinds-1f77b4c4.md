---
title: Infer linear kinds through polymorphism
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:33:27.264362+02:00"
blocks:
  - habu-persist-cad-semantic-028c0881
---

Static invariant: a value whose type may unify with a linear or affine family must not pass through a polymorphic copy/drop path unless the type variable carries and satisfies an explicit copyable/discardable kind constraint. Current concrete-count conservation catches bound linear values but KEEP/over and dup-before-FREE can launder them while a remains unconstrained. Fix: add unrestricted, affine, and linear kind bounds to checker type variables, quotation effects, row-polymorphic words, locals, overload replay, and higher-order execute; copying/discarding primitives impose kind constraints and unification preserves the strongest bound transactionally. Acceptance: polymorphic KEEP/over, dup/drop-before-bind, quotation capture/execute, branch, MATCH, locals, stored-word replay, and nested higher-order laundering reject for linear values; copyable values retain current behavior; valid consume/thread paths certify; diagnostics name the kind constraint; rollback, snapshot, AOT, bootstrap, and fixpoint remain exact. Files: src/core/checker.f, roles/type-variable metadata, focused engine/type-family fixtures, docs/effects.md. Verify: red-first laundering matrix, checker/native/bootstrap/full gates. Serialize after CAD semantic persistence because both own checker metadata; unblocks linear-once resource wrappers and explicit CAD capabilities.
