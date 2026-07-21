---
title: "Infer: fused decode attention kernel"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T15:58:14.142912+02:00"
blocks:
  - habu-infer-gpt-2-412c6f04
---

THE CRUX. Single-query (decode-step) attention: one new token's Q against all cached K/V, online-softmax accumulation, fp16/bf16 storage with f32 accumulate. Phase A: contiguous KV (single sequence) - Q dot K over the cache, running max/sum rescale, V accumulate; element-close vs the GPT-2 host forward's attention at every step of a real decode (the forward dot supplies goldens). Phase B: paged - the kernel walks the block table and gathers pages (TMA bulk fetches per the ISA probe where they win; consumer-Blackwell path); bit-identical results between a contiguous cache and the same tokens scattered across pages IS the acceptance (the paging must be invisible). GQA-ready indexing (n_kv_heads <= n_heads) even though GPT-2 is MHA - the LLaMA family needs it and retrofitting is churn. Legality fail-closed red-first (head-dim/page-size/dtype guards); perf-watch registration + WAIVER rows; correctness-only GPU bursts; decode-throughput timing belongs to the timing lane AFTER correctness lands. Study lib/ptx/cg-attention.f (prefill attention emitter), softmax-rows machinery, and the mma-exact harness discipline before designing.
