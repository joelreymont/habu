---
title: Match GPT-2 device logits
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:40:51.751867+02:00"
blocks:
  - habu-exec-gpt-2-29a09d1a
---

Problem: no device path executes embeddings, all blocks, final normalization, and the tied vocabulary head over real weights. Result: GPT2DEV:LOGITS enqueues each row of one live KV append batch through token/position embedding, exactly config.layer-count reusable GPT2DEV:BLOCK calls, final LayerNorm, the tied vocabulary projection, and the host-visible logit copy into session-owned rows. It never synchronizes, commits, or aborts the KV batch. INFER is the sole caller that synchronizes the session and then commits or aborts KV together with its provisional random/output state; the direct parity fixture performs that same explicit sequence. The GPT-2 milestone calls LOGITS with one row; batching later extends the same operation to several rows. It performs no per-layer allocation, module load, host fallback, tokenizer, sampling, contiguous cache, snapshot/lease, or second forward graph. Owner: complete paged GPT-2 device logits only. Production red: no real GPT-2 model reaches a device logit row. Acceptance: all committed tiny and real probes, selected logits, full-row sums, and greedy identifiers match GPT2-REFERENCE at the recorded positions and page boundaries; no device failure or unsynchronized call mutates committed KV; two sessions reuse persistent owners deterministically; explicit success commits and every injected failure aborts to the last committed tokens with exact ownership. Smallest owning check: bin/hb --load maki/infer/gpt2-device-logits-test.f on DGX Spark. Claim: unassigned.
