---
title: "Infer: sampling ops (temperature, top-k, top-p)"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-21T15:57:38.315510+02:00\""
---

Extend the landed autoregressive sampling (maki/examples/nanogpt/generate.f) into a real sampling module: temperature scaling, top-k (partial selection), top-p/nucleus (sort + cumulative threshold), seeded deterministic RNG (the committed LCG), greedy as the degenerate case. Host-side over the logits span (a vocab row is ~200KB - host read is fine on UMA; Grace does sampling overlapped with the next decode step by design). Red-first: run-twice bit-identical at fixed seed; temperature->0 equals argmax exactly; top-k=1 equals argmax; top-p=1.0 equals plain multinomial; invalid domains (negative temperature, k<1, p outside (0,1]) reject named before any table read. Parity fixture: exact sampled-id sequences vs a torch reference (ml venv, fixed seed, committed with provenance per the adam-torch-ref pattern) on a small synthetic logits table for greedy/top-k/top-p legs.

Claim: agent=sampler workspace=.jj-ws/fable-sampler machine=spark (owns the sampling module + torch parity fixture)
