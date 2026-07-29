---
title: "EPIC: GB10 UMA inference engine (vLLM-class)"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T15:57:38.288053+02:00"
---

Build one checked Habu inference engine for DGX Spark. GPT-2 is the first end-to-end correctness model, not a separate engine. The second model is the pinned Qwen2.5-7B-Instruct checkpoint at revision a09a35458c702b33eeacc393d103063234e8bc28, running through the same model carrier, device session, paged cache, prefill, decode, sampling, and serving loop.

Mandatory order: finish the package and declaration-ownership cut with no owner-WID duplicate authority; hard-cut the unified model types and remove schema versions; finish normalized configuration, tensor, and weight intake; prove the GPT-2 host oracle; build the exact persistent GB10 executor for the operations used by GPT-2; connect the sole KV cache and paged decode path; run GPT-2 through one shared INFER engine; add Qwen2.5 through a second explicit model arm; add continuous batching; add JSON-line and OpenAI-compatible HTTP serving; then measure the real production path. There is no GPT-2 engine, arbitrary tensor-to-PTX framework, plugin ABI, compatibility reader, compiled-pack prerequisite, or second model/config/tensor/cache authority.

Correctness precedes optimization. The first device path uses the simplest correct contiguous and vector-paged kernels. TMA, asynchronous copy, quantization, model packs, device sampling, launch amortization, cache quantization, metrics, and soak work are absent until a production measurement identifies one as the next bottleneck. The performance release gate records the exact source, checkpoint, target, workload, output correctness, latency, throughput, and peak owned memory without a versioned benchmark schema.

This refreeze supersedes the speculative open inference graph recorded at source commit 85a9646fd6b97e5d2cbb86d637bcf8d8ab2aece8. Removed subtrees remain recoverable from that commit. Rejected benchmark candidates d719be252e43 and 785c4021e2e5 remain unmerged because they implement the removed pre-product schema framework. In particular, the compiled-pack, NVFP4, allocation-policy, device-sampling, fast-prefill, launch-amortization, batch-one, small-batch, quantized-dispatch, KV-quantization, broad metrics/soak, TMA, asynchronous-copy, second admission-policy, and benchmark-framework contracts are not implementation work. Re-add only a measured product requirement with a complete owner, real production red, and acceptance path.
