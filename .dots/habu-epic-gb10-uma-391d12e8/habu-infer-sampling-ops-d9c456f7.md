---
title: "Infer: sampling ops (temperature, top-k, top-p)"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-21T15:57:38.315510+02:00\""
---

Extend the landed autoregressive sampling (maki/examples/nanogpt/generate.f) into a real sampling module: temperature scaling, top-k (partial selection), top-p/nucleus (sort + cumulative threshold), seeded deterministic RNG (the committed LCG), greedy as the degenerate case. Host-side over the logits span (a vocab row is ~200KB - host read is fine on UMA; Grace does sampling overlapped with the next decode step by design). Red-first: run-twice bit-identical at fixed seed; temperature->0 equals argmax exactly; top-k=1 equals argmax; top-p=1.0 equals plain multinomial; invalid domains (negative temperature, k<1, p outside (0,1]) reject named before any table read. Parity fixture: exact sampled-id sequences vs a torch reference (ml venv, fixed seed, committed with provenance per the adam-torch-ref pattern) on a small synthetic logits table for greedy/top-k/top-p legs.

Claim: agent=sampler workspace=.jj-ws/fable-sampler machine=spark (owns the sampling module + torch parity fixture)

Review incorporation 2026-07-21 (docs/inference-engine-plan.md sect 3.3): CORRECTION - for a single sequence, token n+1 cannot begin until token n is selected, so host sampling is ON the autoregressive critical path and cannot generally be hidden behind the same sequence's next decode step (overlap only comes from OTHER requests). This module is therefore the CORRECTNESS REFERENCE and early implementation; its per-call latency must be measured and recorded as critical-path cost. A follow-on DEVICE sampler dot (greedy/temperature/top-k/top-p, seeded determinism) owns the latency-sensitive path (habu-infer-device-sampling, M6).
