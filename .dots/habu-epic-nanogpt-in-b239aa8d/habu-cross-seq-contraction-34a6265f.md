---
title: Cross-seq contraction checker reject
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T17:36:22.594997+02:00"
blocks:
  - habu-extent-roles-b-df9d232f
---

Soundness closer for the (B,T,C) fold: minimal checked negative fixture proving a plain MATMUL over folded B*T rows fed where a within-sequence #T contraction is required is a load-time checker reject (exit 70), not a runtime error. Until the extent-role + factorization capabilities land this dot documents the gap (segment-op construction-only enforcement). MUST NOT be closed by a runtime guard - only by the checker rejecting the reduced bad program. Full contract: docs/batch-sequence-design.md section 5 BTC-5.
