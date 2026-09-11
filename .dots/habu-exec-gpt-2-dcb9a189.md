---
title: Execute GPT-2 attention stage
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:50.525110+02:00"
blocks:
  - habu-exec-gpt-2-29bea6c6
  - habu-infer-decode-vector-e5ac69b3
---

Why: paged attention, O projection, and first residual form one independently testable stage. Interface: package-private GPT2DEV:ATTN takes session/weights/layer/activation plus immutable descriptor rows, invokes the sole DECODE-CG vector-paged operation, O projection, and residual, and returns launch state and owners. Owner: GPT-2 attention stage only. Production red: paged attention is not composed with real O weights. Acceptance: GPT2-REFERENCE attention/O/residual probes, scattered/page-edge rows, wrong descriptors, and enqueue failures pass without cache mutation. Forbidden: QKV write, MLP, commit, alternate attention, allocation, or host fallback. Smallest owning check: focused GPT-2 attention-stage test on DGX Spark.
