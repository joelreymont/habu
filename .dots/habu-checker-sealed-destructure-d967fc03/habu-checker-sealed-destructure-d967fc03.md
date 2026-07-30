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

Delete the production-empty OWNER-WID runtime registry, capacity, emit hooks, and trailer first. There are no namespace markers, sink guards, wordlist migrations, or raw-wordlist restrictions in this campaign (Joel, 2026-07-30): the ruled threat model is our own generated code gaming checks, and dictionary surgery or namespace forgery cannot pass diff review, which is the gate it would have to pass. One honest limitation is stated instead of armored against: packages are reopenable by design, so a foreign file reopening the owner package to construct a flagged value is caught by its diff in review, not by a checker theorem.

After the flag works, migrate each retained inference product by adding CONSTRUCT owner and deleting its proof field, NEWTYPE, TRUSTED mint, and proof plumbing in the same hard cut. When the last NEWTYPE consumer is migrated, DELETE the NEWTYPE defining word itself (Joel, 2026-07-30): a proof is then just a family whose construction stayed with its owner, and a separate defining word for the zero-field case is ceremony. No destructure policy, OWNER-WID state, PROT-WID enrollment, family-row growth, compatibility representation, public construction alias, or lint is part of this campaign.

The campaign closes when owner construction works through the production compiler, public products are unchanged, OWNER-WID is absent while PROT-WID remains green, inference proof-mint ceremony is deleted, NEWTYPE no longer exists as a defining word, foreign construction by qualified name rejects at check time, and native fixpoint plus declaration and inference suites pass.
