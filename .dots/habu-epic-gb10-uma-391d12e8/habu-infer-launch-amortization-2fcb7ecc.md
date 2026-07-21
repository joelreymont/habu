---
title: "Infer: launch amortization"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T16:44:54.221678+02:00"
blocks:
  - habu-infer-modern-dense-b4d4aea1
---

Plan-of-record M6: reduce per-token host launch overhead without making model state opaque - measure and implement ONE of: CUDA Graphs keyed by bounded batch+shape, a graph-style driver replay, or a persistent decode loop if graphs cannot express the dynamism. Gate: measured launch overhead before/after; inter-token latency improves; the chosen mechanism documented with its dynamism limits. Blocked on the dense model (real shapes) - frontmatter.
