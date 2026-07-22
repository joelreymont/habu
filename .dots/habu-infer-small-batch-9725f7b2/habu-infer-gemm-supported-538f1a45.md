---
title: "Infer GEMM: supported small-batch geometry"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:41.941738+02:00"
blocks:
  - habu-infer-quant-publish-1457f90e
  - habu-infer-dense-tensor-c037a6fd
---

Why this exists:
The tensor-core path needs explicit supported batch, matrix, packing, activation, and target rows before any dispatch can select it.

Required result:
Define checked small-batch geometry for the pinned dense model and derive tile counts, scale spans, workspace, and launch extents from the pack recipe.

Done when:
All supported rows validate; unsupported batch, divisibility, packing, target, workspace, and overflow reject before emission or launch.

Expected touch points: small-batch geometry and focused tests.
Smallest check: the focused geometry table and rejection test.
Prerequisites: published quantized pack profile and dense-model tensor binding.
Owned result: small-batch GEMM legality and extent derivation only.
Claim: unassigned.
