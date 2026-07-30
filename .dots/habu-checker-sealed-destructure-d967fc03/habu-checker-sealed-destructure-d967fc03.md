---
title: "Checker: owner-only product construction"
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T09:00:47.469920+02:00"
blocks:
  - habu-prove-sealed-inference-1d007ad5
---

Campaign only; do not dispatch. Public products always publish MAKE, so inference owners carry private proof fields and TRUSTED mint words solely to prevent foreign construction. Replace that ceremony with one compiler rule: a STRUCTURE may declare CONSTRUCT owner, recorded as one flag in the existing TF.DERIVE word. Unflagged products retain their ordinary public MAKE and UNMAKE. An owner product publishes no MAKE; its original package constructs it through the existing compiler form extended to products. UNMAKE remains ordinary because destructuring plain fields cannot mint the nominal value.

Delete the production-empty OWNER-WID runtime registry, capacity, emit hooks, and trailer first. Once a package owns an owner-construction product, append one same-name XREF namespace marker with DNAME-INT and zero role fields. Ordinary namespace lookup skips markers. The native package and definition sinks reject the marked name and its canonical public or private wordlist identifier, and XREF plus dictionary truncation cannot remove the marker. Keep the live PROT-WID registry and protected-memory enforcement for compiler internals unchanged.

Expose CONSTRUCT owner syntax only after the marked namespace, both native guard leaves, declaration rollback, and evaluate rollback are complete. Then migrate each retained inference product by adding CONSTRUCT owner and deleting its proof field, NEWTYPE, TRUSTED mint, and proof plumbing in the same hard cut. No destructure policy, OWNER-WID state, PROT-WID enrollment, family-row growth, schema version, compatibility representation, public construction alias, raw-wordlist migration, or lint is part of this campaign. Candidate commits 5fa4a6763dab, 321b9fb1bdd0, 41afe26c, and 3c8bc494 are rejected; their old contracts remain recoverable at source commit 85a9646fd6b97e5d2cbb86d637bcf8d8ab2aece8.

The campaign closes when owner construction works through the production compiler, public products are unchanged, owner packages cannot reopen, OWNER-WID is absent while PROT-WID remains green, inference proof-mint ceremony is deleted, foreign reconstruction rejects before lowering, and native fixpoint plus declaration and inference suites pass.

Current phase anchor (2026-07-30): hard-cut owner-only construction and delete OWNER-WID without compatibility, a new lint, or unrelated naming churn.
