---
title: Model GPU contractions
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:03:22.114962+02:00"
blocks:
  - habu-validate-gpu-reduction-ff746ff8
---

Full context: design GPU Wave D adds logical contraction, operand/layout domains, accumulation/numeric policy, and epilogue fusion to GPU-KIR without schedule details. Acceptance: shape/layout/dtype/accumulator/epilogue mutations reject; canonical GEMM/MMA logical fixtures bind source and target.
