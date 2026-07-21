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

Review incorporation 2026-07-21 (docs/inference-engine-plan.md M3): this is a KERNEL FAMILY, designed from the start for variants across contiguous/paged, MHA/GQA/MQA, head dim, page size, dtype (bf16/fp16, later quantized KV), single vs small-batch decode, short vs long context, prefix-sharing tables. TMA is an AUTOTUNED CANDIDATE, not an assumption - compare vectorized global loads and async copies under the same harness before committing. Context-regime benchmarks (short/medium/long) required at the gate. Batched decode (stage C: bounded batch, ragged lengths, completed-sequence masking, no host per-head launch loop) is the follow-on dot habu-infer-batched-paged.
