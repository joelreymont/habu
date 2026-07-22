---
title: "Infer quant dispatch: shape selection"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:42.785633+02:00"
blocks:
  - habu-infer-quant-dispatch-5e7b735e
  - habu-infer-gemv-batch-cc4871e3
  - habu-infer-gemm-small-5c717119
---

Why this exists:
Each projection call must choose batch-one, small-batch, or BF16 from measured schedule rows without hidden fallback.

Required result:
Select a registered kernel from the exact site, batch, shape, target, and profile key. Return a named unsupported result when no measured row exists.

Done when:
Boundary batches select the declared row; changing any key changes or rejects the choice; no missing low-bit row silently falls back to another implementation.

Expected touch points: quantized dispatch module and focused table tests.
Smallest check: the focused dispatch-boundary test.
Prerequisites: quantized-site registry and both kernel benchmark tables.
Owned result: deterministic shape-keyed selection only.
Claim: unassigned.
