---
title: "Infer: device sampling"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T16:44:32.526672+02:00"
blocks:
  - habu-infer-end-to-20fa7684
---

Plan-of-record M6: host sampling is on the single-sequence critical path (the sampling dot's close records the correction), so the latency-sensitive path gets a device sampler: greedy, temperature, top-k, top-p (or a bounded approximation with documented semantics), seeded deterministic where semantics require. Baseline FIRST: measure the host module's per-call latency (maki/sampling.f) as critical-path cost; the device sampler must improve or preserve inter-token latency, measured. Semantics must match the host reference exactly on the deterministic legs; CPU/GPU sync cost measured. Kernel-perf watch registration per convention.
