---
title: "Infer M0: lightweight baseline adapter"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.777486+02:00"
blocks:
  - habu-infer-m0-benchmark-c50501b7
  - habu-infer-m0-adapter-2965b6c9
  - habu-infer-m0-gb10-e490b7d3
---

Why this exists:
M0 requires a second lighter engine baseline on the same workload schema and checkpoint where its format permits.

Required result:
add a reproducible llama.cpp adapter with pinned version and flags, explicit model-format compatibility, metric capture, and clean shutdown.

Done when:
unsupported model format is an explicit result; wrong version, readiness failure, malformed output, timeout, and nonzero exit reject named; fixture and presence-gated live smoke pass.

Expected touch points: the llama.cpp adapter under tools/infer-bench/ and focused fixture and live tests.
Smallest check: the focused adapter fixtures and one presence-gated live smoke test.
Prerequisites: workload matrix, adapter execution protocol, and GB10 hardware manifest.
Owned result: lightweight-engine adapter only.
Claim: unassigned.
