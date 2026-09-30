---
title: Package words spelling op rows differ by tier
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T18:14:00.939171+02:00"
---

Found by the Fable review of the sealed vocabulary (habu-load-authored-src-4ef714a3), 2026-09-30. Unsealed, a package's public word whose name matches a JIT op row (for example MIN or 2DUP; LKWCMP folds A-Z, so MIN matches row min) means the op at tier 0, because LKWCMP runs before any dictionary lookup, and the package's word at tier 1, which binds it through ROW-BOUND?/INTRINSIC-BOUND? (src/compiler/native/hir-word.f:942-949). The same source therefore behaves differently by tier. The same holds for a user definition spelled like an op row (: dup ( -- n ) 5 ;): definable, uncallable at tier 0, callable at tier 1. The sealed vocabulary refuses both under the seal only. Recommendation from the review: after a census of lib/, Etch and Maki, make op-row spellings E-RESERVED-DEFINITION at both tiers engine-wide, with the row-union walk generated from the emitter's own tables (the sealed guard builds that walk).
