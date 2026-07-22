---
title: "Infer ops: restart reproducibility"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:45.712971+02:00"
blocks:
  - habu-infer-serve-one-c0c151d2
  - habu-infer-ops-metric-3d96abe2
---

Why this exists:
Starting from the same pack and configuration must reconstruct the same engine identity and initial capacity plan.

Required result:
Record the complete startup identity and compare two fresh-process starts before accepting traffic.

Done when:
Matching starts produce the same pack, target, kernel, schedule, capacity, and metric identities; changing any input changes or rejects the identity; no stale cache silently substitutes another artifact.

Expected touch points: startup identity record and focused fresh-process tests.
Smallest check: two-start identity parity and one changed-input negative.
Prerequisites: one-command serving and operational metric schema.
Owned result: startup identity and reproducibility proof only.
Claim: unassigned.
