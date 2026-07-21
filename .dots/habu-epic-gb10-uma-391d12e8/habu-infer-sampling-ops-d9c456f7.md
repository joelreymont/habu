---
title: "Infer: sampling ops (temperature, top-k, top-p)"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-21T15:57:38.315510+02:00\\\"\""
closed-at: "2026-07-21T16:44:02.464981+02:00"
close-reason: "Landed b39e7dae: the canonical host sampling module (maki/sampling.f) - temperature/top-k/top-p over a logits span with named domain guards firing before any table read, exact-argmax short circuits, f64 max-subtraction softmax, caller-owned-scratch heap for nucleus (allocation-free per call), the committed LCG, and documented deterministic tie-breaking. Torch 2.9.1 parity fixture with provenance: deterministic legs exact-id vs torch, stochastic legs pin the committed RNG's ids and cross-check the distribution (4096-draw histogram within 0.006 of torch probs); softmax parity measured 4.6e-10. Six red-first guard rejects; run-twice locked. REVIEW CORRECTION carried into the close: this module is the CORRECTNESS REFERENCE - for a single sequence host sampling sits ON the autoregressive critical path and cannot hide behind the same sequence's next decode; the latency-sensitive path belongs to the device-sampling dot, whose baseline includes measuring this module's per-call latency. Follow-up dotted: fold generate.f's inline sampling copies onto this module. Full tests green at the merged tip"
---

Extend the landed autoregressive sampling (maki/examples/nanogpt/generate.f) into a real sampling module: temperature scaling, top-k (partial selection), top-p/nucleus (sort + cumulative threshold), seeded deterministic RNG (the committed LCG), greedy as the degenerate case. Host-side over the logits span (a vocab row is ~200KB - host read is fine on UMA; Grace does sampling overlapped with the next decode step by design). Red-first: run-twice bit-identical at fixed seed; temperature->0 equals argmax exactly; top-k=1 equals argmax; top-p=1.0 equals plain multinomial; invalid domains (negative temperature, k<1, p outside (0,1]) reject named before any table read. Parity fixture: exact sampled-id sequences vs a torch reference (ml venv, fixed seed, committed with provenance per the adam-torch-ref pattern) on a small synthetic logits table for greedy/top-k/top-p legs.

Claim: agent=sampler workspace=.jj-ws/fable-sampler machine=spark (owns the sampling module + torch parity fixture)

Review incorporation 2026-07-21 (docs/inference-engine-plan.md sect 3.3): CORRECTION - for a single sequence, token n+1 cannot begin until token n is selected, so host sampling is ON the autoregressive critical path and cannot generally be hidden behind the same sequence's next decode step (overlap only comes from OTHER requests). This module is therefore the CORRECTNESS REFERENCE and early implementation; its per-call latency must be measured and recorded as critical-path cost. A follow-on DEVICE sampler dot (greedy/temperature/top-k/top-p, seeded determinism) owns the latency-sensitive path (habu-infer-device-sampling, M6).
