---
title: Execute GPT-2 QKV stage
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:50.404909+02:00"
blocks:
  - habu-infer-kv-atomic-cdfb00cb
---

Why: GPT-2 input normalization, QKV projection, and provisional K/V write form one independently testable device stage. Interface: package-private GPT2DEV:QKV consumes immutable row descriptors plus session/weights/layer/activation owners, launches exact LayerNorm and QKV, and writes K/V only at authenticated provisional coordinates; it returns DEVRT launch state and owners, never KV authority. Owner: GPT-2 QKV stage only. Production red: no real device stage writes the paged descriptor. Acceptance: GPT2-REFERENCE first/last-layer QKV/KV probes, page edge, wrong roles/extents, and injected enqueue failure preserve owners. Forbidden: attention, O projection, MLP, commit, allocation, host fallback, or second descriptor. Smallest owning check: focused GPT-2 QKV device test on DGX Spark.
