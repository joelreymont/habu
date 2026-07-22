---
title: "Infer quant dispatch: site registry"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:42.639047+02:00"
blocks:
  - habu-infer-quant-publish-1457f90e
  - habu-infer-gemv-real-db86a0b7
  - habu-infer-gemm-projection-a7acb2e1
---

Why this exists:
Runtime dispatch needs one immutable mapping from model projection sites and shapes to compatible packed members, kernel families, quality profiles, and schedules.

Required result:
Build and validate the quantized-site registry from the loaded model pack before engine publication.

Done when:
Every required site has exactly one compatible row; duplicate, missing, stale profile, unsupported shape, and unknown kernel keys reject before the engine becomes visible.

Expected touch points: quantized-site registry and focused tests.
Smallest check: the focused complete and conflicting registry test.
Prerequisites: published quantized pack profile, batch-one integration, and small-batch epilogue integration.
Owned result: immutable quantized-site registry only.
Claim: unassigned.
