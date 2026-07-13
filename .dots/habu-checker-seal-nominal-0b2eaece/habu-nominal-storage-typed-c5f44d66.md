---
title: "Nominal storage: typed definers"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T16:08:46.264849+02:00"
blocks:
  - habu-nominal-storage-effect-a60ba885
---

Problem: landed LAYOUT-BUFFER already stores closed non-linear layouts and arity-zero nominal scalars with checked fixed-capacity indexing, typed access, overflow, zeroing, rollback, snapshot, and fixpoint coverage. The residual gap is convenience definers: no sound uppercase TYPED-VARIABLE/TYPED-BUFFER surface exists for typed scalar/pointer storage outside a fixed LAYOUT-BUFFER owner. Acceptance: add those definers without weakening landed LAYOUT-BUFFER; admit closed addressable arity-zero families, nominal atoms, closed layout families and closed typed pointers; reject open variables, quotations, linear values, hidden fields, unresolved args, duplicates, and unconstrained typed create; typed constants require a checked producer. Pin a live numeric-result<nominal> LAYOUT-BUFFER positive and raw-variable laundering negative so the two capabilities remain distinct. Files: src/core/checker.f, a focused storage-definer owner, native/bootstrap grammar if required, engine/storage tests, docs/type-families.md. Verify: same-family storage positives, cross-family/bounds/overflow/rollback negatives, native fixpoint/bootstrap/full gates. Depends: habu-nominal-storage-effect-a60ba885. CAD-NUM B5 does not depend on this convenience surface.
