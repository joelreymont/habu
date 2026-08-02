---
title: Prove sealed inference
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:36:50.572569+02:00"
blocks:
  - habu-seal-validated-model-4361ff22
  - habu-seal-gpt-2-1426e51e
  - habu-parse-gpt-2-c8baa4db
  - habu-own-gpt-2-14415dcd
---

Problem: isolated migration tests cannot prove the complete declaration,
package, compiler, and inference boundary after hard cutover. The production
defect is that `MDLCFG` and `GPT2TENSOR` require construction ceremony while a
foreign caller can reconstruct their products.

Result: one production-path acceptance loads the retained `MDLCFG` and
`GPT2TENSOR` declarations from source, replays them, rebuilds native `hb` to a
fixpoint, parses the pinned GPT-2 config through `HFCFG`, and exercises tensor
layer creation and lookup. Drive the real `CONSTRUCT` behavior: owner-side
construction succeeds, `UNMAKE` remains available where specified, and a
foreign attempt to call or recreate the generated `MAKE` surface rejects
before lowering or mutation with the named diagnostic.

Dependencies are the retained MDLCFG/GPT2TENSOR migrations and the declaration
and evaluate-rollback owners they use. Owner: focused acceptance fixtures and
exact public-symbol assertions only; no runtime repair. Do not add a proof
field, trusted mint, owner marker, persistence payload, copied WID role, schema
version, compatibility path, rejected-source allowlist, destructure policy,
raw-wordlist migration, or temporary boundary.

Acceptance: valid pinned config and tensor lookup pass twice; foreign `MAKE`
reconstruction rejects through the production compiler path; source load,
replay, AOT, snapshot, and fixpoint all preserve the same `CONSTRUCT` behavior
and public surface. Run declaration, transaction, package, inference, exact
diff, Maki, and full native gates.

Claim: unassigned.
