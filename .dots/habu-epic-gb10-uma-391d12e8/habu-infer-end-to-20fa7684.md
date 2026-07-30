---
title: "Infer: end-to-end single-sequence engine"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T15:58:37.094041+02:00"
blocks:
  - habu-infer-engine-64-02416606
---

Campaign only; do not dispatch. Its leaves own the explicit model carrier, engine lifetime, sequence rows, one-token prefill, sampling, and exact 64-token GPT-2 acceptance. The engine owns one persistent model session and one multi-sequence device KV cache. Prefill and decode both use the sole RUN-ROWS transaction; NEXT-MANY adds sampling and output publication around it. No engine-per-request wrapper, contiguous product cache, pack, plugin, vtable, host fallback, benchmark, metrics framework, or second engine belongs here. Close when one command directly loads the real GPT-2 device weights, opens an engine and sequence, emits the exact 64 reference identifiers and bytes twice, and releases every owner.
