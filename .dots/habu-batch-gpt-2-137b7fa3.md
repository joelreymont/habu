---
title: Batch GPT-2 dense rows
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T23:22:20.105651+02:00"
blocks:
  - habu-infer-batch-decode-a7520e15
  - habu-exec-gpt-2-29a09d1a
---

Why: continuous batching requires the non-attention GPT-2 kernels to index active rows instead of a host loop. Result: extend the existing GPT2DEV embedding, LayerNorm, linear, GELU, residual, final norm, and vocabulary-head operations with one active-row count from one through four and the engine-owned row descriptor; each launch covers all active rows and preserves existing batch-one results. Dependencies: the persistent GPT-2 device block and INFER row descriptor. Owner: row indexing in GPT2DEV non-attention operations only. Production red: those operations address one activation row. Acceptance: batches one, two, and four with mixed sequence positions match independent row results; all three issue the same operation-launch count; zero, five, wrong, duplicate, short, or out-of-range rows reject before launch. Forbidden: per-row host loop, new kernel family, scheduler state, KV mutation, allocation, fallback, or benchmark framework. Smallest owning check: bin/hb --load maki/infer/gpt2-device-dense-batch-test.f on DGX Spark.
