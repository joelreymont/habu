---
title: "Infer: M0 measurement contract + baselines"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-21T16:44:32.497176+02:00\""
---

Plan-of-record M0 (docs/inference-engine-plan.md): no public performance claim exists before this does. Deliver: hardware/driver/runtime manifest; reproducible baseline scripts for vLLM (best reproducible GB10 config, not default-install fallback kernels) and one lighter engine (llama.cpp where the format permits) on the exact target checkpoint + prompt suite; a machine-readable benchmark schema capturing the full protocol (commit, checkpoint+pack checksums, driver/CUDA/ptxas versions, precision, prompt/output lengths, concurrency, sampling mode, cold/warm, min/median/p95/runs, peak unified memory, post-warmup page faults, baseline versions+flags); the core benchmark matrix (interactive-short, coding/RAG 4K, long-prompt 32K, small-team mixed x4, KV-pressure, soak). Required metrics per the doc: cold/warm load, TTFT, inter-token latency, decode+prefill tok/s, peak memory, page-fault activity, CPU util, KV bytes/token, max safe aggregate tokens.

Claim: agent=m0bench workspace=.jj-ws/fable-m0bench machine=spark (owns the M0 measurement contract: manifest, baseline scripts, schema, first numbers; SOLE timing lane while active)
