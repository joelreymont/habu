---
title: Delete unused config identity
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T21:49:05.610490+02:00"
blocks:
  - habu-own-gpt-2-14415dcd
  - habu-delete-model-semantic-deac6398
  - habu-delete-gpt-2-7fa753dd
  - habu-delete-model-config-1c71a13e
---

Problem: MDLCFG computes a 32-byte cfgkey and GPT2TENSOR copies it into each layer-id even though every product lookup receives the exact consuming MDLCFG value. Result: after the single GPT2TENSOR catalog lands, delete cfgkey, CONTENT-KEY folding and scratch, CFGKEY@/CFGKEY=, layer-id.key, every key argument/projection/test, and the content-key require if unused. Keep one nominal layer index minted by GPT2TENSOR and range-check it against the consuming MDLCFG on every lookup. Owner: MDLCFG identity and GPT2TENSOR layer identity only. Production red: equal configs create identity plumbing that does not enforce a product invariant. Acceptance: no cfgkey, key-fold, or layer-id.key symbol remains; old arities reject; layer zero and last work while wrong and one-over indices reject through SLOT; HFCFG, GPT2TENSOR, and direct GPT2DEV intake stay exact; focused config, tensor, package, trust, and diff gates pass. Forbidden: replacement digest, registry, hidden token, format identifier, version, compatibility field, or lint. Claim: unassigned.
