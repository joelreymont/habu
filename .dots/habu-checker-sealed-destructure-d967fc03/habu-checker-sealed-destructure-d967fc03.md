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

Once a package owns an owner-construction product, the compiler rejects reopening it. Raw wordlist operations become trusted-only so a saved numeric identifier cannot bypass the package form. Delete the production-empty OWNER-WID runtime registry, capacity, emit hooks, and trailer without replacement. Keep the live PROT-WID registry and protected-memory enforcement for compiler internals.

Migrate each retained inference product by adding CONSTRUCT owner and deleting its proof field, NEWTYPE, TRUSTED mint, and proof plumbing in the same hard cut. No destructure policy, owner-WID, family-row growth, schema version, compatibility representation, public construction alias, or runtime protection survives. Candidate commits 5fa4a6763dab, 321b9fb1bdd0, 41afe26c, and 3c8bc494 are rejected; their old contracts remain recoverable at source commit 85a9646fd6b97e5d2cbb86d637bcf8d8ab2aece8.

The campaign closes when owner construction works through the production compiler, public products are unchanged, owner packages cannot reopen, OWNER-WID is absent while PROT-WID remains green, inference proof-mint ceremony is deleted, foreign reconstruction rejects before lowering, and native fixpoint plus declaration and inference suites pass.
