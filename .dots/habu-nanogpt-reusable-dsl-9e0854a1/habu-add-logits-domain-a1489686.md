---
title: Add logits-domain CE device lowering
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:30:31.368448+02:00"
blocks:
  - habu-make-ptx-device-c0eb12a3
  - habu-fix-ptx-collective-997cfcce
  - habu-ad-softmax-rows-8c9552fb
  - habu-ptx-ad-device-2b511851
  - habu-add-ptx-planner-30b93e8c
---

File: PLAN.md:390; cause: maki/celoss.f owns CPU CE, but the GPU path lacks a generic logits-domain fused softmax-cross-entropy lowering and device gradcheck over integer class ids; fix: add checked PTX/Maki lowering for logsumexp CE, p-onehot backward, target-range validation, shape rejection, and device finite-difference tests; deps: fail-closed device runtime, fixed collectives, softmax backward, device gradcheck harness, planner; verification: CE CPU numeric tests and Orin ce-loss-device-test cover huge logits, equal logits, target 0, target vocab-1, -1, vocab, malformed one-hot, and profile rows.
