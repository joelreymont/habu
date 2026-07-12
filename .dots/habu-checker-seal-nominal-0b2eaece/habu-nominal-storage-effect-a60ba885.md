---
title: "Nominal storage: effect parametricity"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T16:08:35.684126+02:00"
blocks:
  - habu-nominal-storage-raw-a3430ef2
---

Problem: a declared polymorphic effect can specialize a variable to a concrete family or erase ptr family to ptr a, then the checker records the original generic signature. Acceptance: after body checking, every declared quantifier still resolves to a distinct variable; specialization or quantifier aliasing rejects; valid generic LOAD and ID wrappers certify; pointer-pointee diagnostic path and E-NONPARAMETRIC-EFFECT repair class are stable; multi-error and rollback never persist rejected signatures. Files: src/core/checker.f, diagnostic renderer/schema, engine and all-errors fixtures, docs/effects.md. Verify: direct family-to-a, ptr-family-to-ptr-a and injectivity negatives; generic positives; checker/rollback/fixpoint/bootstrap/full gates. Depends: habu-nominal-storage-raw-a3430ef2.
