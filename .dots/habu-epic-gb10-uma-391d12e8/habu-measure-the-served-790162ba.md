---
title: Record served measurements
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:19:13.320075+02:00"
blocks:
  - habu-capture-served-samples-bfa4fcf2
---

Why: raw product-path samples need one human-readable reduction, not a benchmark framework. Result: consume the checked capture output and write one section in docs/inference-performance.md containing immutable source commit, UTC time, device, driver, toolchain, CPU and OS facts, exact model pins, every raw latency and generated-token-rate sample, the fifth sorted GPT-2 sample, the second sorted Qwen sample, and exact load and steady-state FOOTPRINT rows including staging high-water. State no time-to-first-token or inter-token result for the non-stream endpoint. Owner: one documentation reduction only. Production red: the served paths have no recorded measurement. Acceptance: every recorded value traces to one capture row, sorted selections are exact, owned bytes are not inferred from global free memory, and the largest observed bottleneck is identified with at most one evidence-backed product dot. Forbidden: measurement command, percentile framework, schema, manifest, adapter, database, threshold, gate, dashboard, generated artifact, generic harness, version, or optimization work. Smallest owning check: regenerate the section from one accepted DGX Spark capture. Claim: unassigned.
