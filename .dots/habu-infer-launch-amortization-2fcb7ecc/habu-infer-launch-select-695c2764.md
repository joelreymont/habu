---
title: "Infer launch: select replay mechanism"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.790474+02:00"
blocks:
  - habu-infer-launch-measure-6c070a3a
---

Why this exists:
CUDA graphs, driver replay, and a persistent loop have different dynamism limits; choosing before measuring risks architecture churn.

Required result:
probe each viable mechanism against the exact model state and batch/shape changes, then freeze one explicit contract.

Done when:
the decision records supported mutations, capture/update failures, ownership, cancellation, and measured overhead; mechanisms that cannot represent required dynamism are rejected with evidence.

Expected touch points: focused Habu probes and decision record.
Smallest check: presence-gated GB10 probes.
Prerequisites: measure host overhead.
Owned result: mechanism evaluation and frozen contract only.
Claim: unassigned.
