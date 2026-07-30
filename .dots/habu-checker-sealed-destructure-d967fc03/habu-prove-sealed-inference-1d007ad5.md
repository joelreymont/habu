---
title: Prove owner-only inference
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

Problem: isolated migration tests cannot prove the complete declaration, package, compiler, and inference boundary after hard cutover. Result: one production-path acceptance loads the owner-construction declarations from source, replays them, rebuilds native hb to a fixpoint, parses the pinned GPT-2 config through HFCFG, exercises GPT2TENSOR layer creation and lookup, then attempts foreign reconstruction, owner-package reopen, saved-public-WID publication, saved-private-WID publication, marker retirement, and truncation through the production compiler and runtime sinks. Dependencies are the retained MDLCFG and GPT2TENSOR migrations; their owner-product surface transitively requires OWNER-WID deletion, the XREF marker, both native guard leaves, and evaluate rollback. Prove the family row remains 19 cells with TF.DERIVE at offset 18, each owner package has one canonical namespace followed by one same-name zero-role DNAME-INT marker, the ordinary image has no OWNER-WID trailer, and PROT-WID remains functional. Public-symbol inventories lose only owner-product MAKE words, proof/mint names, and OWNER-WID words; UNMAKE remains. No proof field, trusted mint, OWNER-WID state, copied WID roles, schema version, compatibility path, rejected-source allowlist, destructure policy, raw-wordlist migration, or temporary boundary remains. Owner: acceptance fixtures and exact inventories only; no runtime repair. Production red: MDLCFG and GPT2TENSOR require proof ceremony while foreign code can reconstruct them. Acceptance: every hostile fixture rejects before lowering or mutation with a named diagnostic; valid pinned config and tensor lookup pass twice; source, AOT, snapshot, and fixpoint preserve namespace order and marker identity exactly; old OWNER-WID bytes are not consumed; PROT-WID tests stay green; declaration, transaction, package, trust, inference, and exact diff gates pass. Claim: unassigned.
