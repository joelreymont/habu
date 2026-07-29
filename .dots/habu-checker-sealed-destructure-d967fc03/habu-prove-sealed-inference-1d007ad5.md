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
  - habu-restrict-raw-wordlist-66b28625
  - habu-reject-owner-pkg-938a7d15
---

Problem: isolated migration tests cannot prove the complete declaration, package, compiler, and inference boundary after hard cutover. Result: one production-path acceptance loads the owner-construction declarations from source, replays them, rebuilds native hb to a fixpoint, parses the pinned GPT-2 config through HFCFG, exercises GPT2TENSOR layer creation and lookup, then attempts foreign reconstruction, owner-package reopen, and saved-wordlist bypasses through the production compiler. Dependencies: the retained MDLCFG and GPT2TENSOR migrations plus OWNER-WID deletion. It proves the family row remains 19 cells with TF.DERIVE at offset 18 and the ordinary image loses only the OWNER-WID trailer portion without a replacement; PROT-WID remains functional. Public-symbol inventories lose only owner-product MAKE words, proof/mint names, and OWNER-WID words; UNMAKE remains for readable public products. No proof field, trusted mint, owner-WID table, schema version, compatibility path, rejected-source allowlist, destructure policy, or temporary boundary remains. Owner: acceptance fixtures and exact inventories only; no runtime repair. Production red: MDLCFG and GPT2TENSOR require proof ceremony while foreign code can reconstruct them. Acceptance: every hostile fixture rejects with a named checker diagnostic before lowering, valid pinned config and tensor lookup pass twice, old OWNER-WID trailer bytes are not consumed, PROT-WID tests stay green, and declaration/transaction/package/trust/native fixpoint and exact diff gates pass. Claim: unassigned.
