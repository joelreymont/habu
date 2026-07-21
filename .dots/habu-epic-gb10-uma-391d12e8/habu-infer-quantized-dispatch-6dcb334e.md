---
title: "Infer: quantized dispatch + e2e measure"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T16:45:21.603271+02:00"
blocks:
  - habu-infer-batch-one-6e608ae5
  - habu-infer-small-batch-9725f7b2
---

Plan-of-record M8 split (4 of 4): shape-keyed dispatch choosing GEMV vs GEMM per site, wired into the decode loop; end-to-end measured vs the bf16 baseline on the quiet box per the M0 protocol. The go/no-go from the doc: the quantized flagship beats the best reproducible baseline by a material margin (provisionally 20%) or materially expands safe context/concurrency at comparable latency; fourfold byte reduction is NEVER advertised as fourfold throughput. Blocked on both split kernels - frontmatter.
