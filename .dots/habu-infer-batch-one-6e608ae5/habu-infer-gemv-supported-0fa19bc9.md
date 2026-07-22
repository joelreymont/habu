---
title: "Infer GEMV: supported batch-one geometry"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:41.231660+02:00"
blocks:
  - habu-infer-quant-publish-1457f90e
  - habu-infer-dense-tensor-c037a6fd
---

Why this exists:
The batch-one low-bit path needs a closed set of supported matrix shapes, packing rows, alignments, and target capabilities before kernel launch.

Required result:
Define checked batch-one geometry for every quantized projection site in the pinned dense model and derive all strides, group counts, and launch extents from the pack recipe.

Done when:
Every supported site validates; wrong packing, alignment, scale count, target, or overflow rejects before emission or launch.

Expected touch points: batch-one geometry and focused tests.
Smallest check: the focused geometry table and rejection test.
Prerequisites: published quantized pack profile and dense-model tensor binding.
Owned result: batch-one GEMV legality and extent derivation only.
Claim: unassigned.
