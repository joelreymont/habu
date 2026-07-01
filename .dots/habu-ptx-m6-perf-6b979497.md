---
title: "PTX M6 perf: warp-shfl block reduction"
status: open
priority: 3
issue-type: task
created-at: "2026-06-27T10:55:04.731339+02:00"
blocks:
  - habu-make-ptx-device-c0eb12a3
  - habu-fix-ptx-collective-997cfcce
  - habu-habu-native-kernel-548b0d4c
---

File: PLAN.md:390. Gap: `BLOCK-MAX`/`BLOCK-SUM` still use a correct but
O(B) thread-0 fold, so reduction-heavy softmax/attention can be bandwidth- and
latency-poor even after correctness lands. Fix: replace with warp-shuffle
membermask reductions, per-warp shared staging, final warp reduce, and preserve
op-local inactive identity semantics from the collective fix. Verify: direct row
sum, softmax, and attention goldens stay green on Orin and generic profile rows
show the reduction bandwidth/latency improvement.
