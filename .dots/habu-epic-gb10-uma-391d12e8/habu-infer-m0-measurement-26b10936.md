---
title: "Infer: M0 measurement contract + baselines"
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-21T16:44:32.497176+02:00\""
blocks:
  - habu-infer-m0-pinned-17b6e648
---

This is the M0 measurement campaign record. Do not dispatch it as implementation work. Its leaves own the benchmark schema, workload matrix, adapter protocol, statistical reducer, DGX Spark hardware manifest, vLLM and lightweight-engine adapters, unified-memory counters, and pinned baseline results.

The existing `.jj-ws/fable-m0bench` work remains preserved for leaf-by-leaf review. Shell and other host tools are allowed; the review must decide whether its process ownership, failure reporting, reproducibility, and telemetry satisfy each leaf contract.

Claim: unassigned.
