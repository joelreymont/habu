---
title: "Infer launch: measure host overhead"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.784509+02:00"
blocks:
  - habu-infer-dense-full-14833530
  - habu-infer-batch-decode-7df51c5c
---

Why this exists:
launch amortization has no measured breakdown for driver calls, synchronization, sampling, and kernels on the pinned model.

Required result:
record a per-token launch trace and median/p95 overhead under the M0 schema.

Done when:
trace accounts for every launch and synchronization without changing execution; results cover batch one and supported small batch.

Expected touch points: launch trace tool and canonical result.
Smallest check: trace fixture and schema validation.
Prerequisites: modern full BF16 parity and batched decode parity.
Owned result: launch overhead measurement only.
Claim: unassigned.
