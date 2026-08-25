---
title: "EPIC: GB10 UMA inference engine (vLLM-class)"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T15:57:38.288053+02:00"
blocks:
  - habu-measure-the-served-790162ba
---

Build one checked Habu inference engine for DGX Spark. GPT-2 is the first end-to-end correctness model, not a separate engine. The second model is the pinned Qwen2.5-7B-Instruct checkpoint at revision a09a35458c702b33eeacc393d103063234e8bc28, running through the same model carrier, device session, paged cache, prefill, decode, sampling, and serving loop.

Mandatory order: finish package-owned construction and hard-cut the unified model types; finish normalized configuration and the GPT-2 tensor catalog; pin the GPT-2 assets and tokenizer; build the exact persistent GB10 operations and direct checkpoint loader; connect the sole KV cache and paged decode path; run GPT-2 through one shared INFER engine against the committed GPT2-REFERENCE outputs; then complete two product branches that join at serving: add Qwen2.5 through a second explicit model arm, and add real GPT-2 continuous batching through that engine. Serve both arms through the bounded OpenAI-compatible HTTP endpoint, then measure that exact production path. There is no host GPT-2 engine, arbitrary tensor-to-PTX framework, plugin ABI, compatibility reader, compiled-pack prerequisite, JSON-line transport, or second model/config/tensor/cache authority.

Correctness precedes optimization. The product uses the simplest correct vector-paged decode path. Tensor Memory Accelerator work, asynchronous copy, quantization, model packs, device sampling, launch amortization, cache quantization, metrics, and soak work are absent until production measurement identifies one next bottleneck. The final measurement records exact source, checkpoint, target, workload, output correctness, request latency, throughput, and owned memory without a benchmark schema.
