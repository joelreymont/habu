---
title: "Infer M0: vLLM baseline adapter"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.772454+02:00"
blocks:
  - habu-infer-m0-benchmark-c50501b7
  - habu-infer-m0-adapter-2965b6c9
  - habu-infer-m0-gb10-e490b7d3
---

Why this exists:
M0 requires the best reproducible GB10 vLLM baseline, but the current adapter does not prove which backend served the request and does not fail closed on fallback kernels.

Required result:
add a reproducible process adapter that starts the pinned vLLM configuration, proves the requested model and backend are active, drives one benchmark cell, captures metrics and logs, and shuts down cleanly.

Done when:
wrong version, fallback backend, readiness failure, malformed response, timeout, and nonzero exit reject named; fixture server and one live smoke pass.

Expected touch points: the vLLM adapter under tools/infer-bench/ and focused fixture and live tests.
Smallest check: the focused adapter fixtures and one presence-gated live smoke test.
Prerequisites: workload matrix, adapter execution protocol, and GB10 hardware manifest.
Owned result: vLLM process adapter only.
Claim: unassigned.
