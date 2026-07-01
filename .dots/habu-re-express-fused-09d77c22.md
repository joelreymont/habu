---
title: "Re-express fused attention as a checked KERNEL: body"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T18:10:56.003236+02:00"
blocks:
  - habu-checker-capability-typed-e0c76a02
  - habu-make-ptx-device-c0eb12a3
  - habu-ptx-m5-mask-eb0716f1
  - habu-fix-ptx-collective-997cfcce
---

File: PLAN.md:390. Gap: `lib/ptx/cg-attention.f` uses unchecked raw PTX
islands around online softmax/attention. Fix: rewrite the attention body as a
checked `KERNEL:` using typed loop, shared-tile, accumulator, causal-mask, and
collective words, then delete or narrow the unchecked boundary. Verify: checker
certifies the body, emitted PTX is device-correct for causal goldens, and
future-token sentinel negatives still reject.
