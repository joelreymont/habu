---
title: "Infer ops: metric accumulator"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:45.487612+02:00"
blocks:
  - habu-infer-ops-metric-3d96abe2
  - habu-infer-serve-engine-37b8873a
---

Why this exists:
Runtime updates need exact ownership and reset semantics so concurrent requests cannot double-count or corrupt latency distributions.

Required result:
Maintain the declared counters, gauges, and bounded latency summaries from scheduler, engine, cache, and kernel events under one generation-bearing accumulator.

Done when:
Deterministic traces produce exact values; duplicate and stale events reject; counter overflow, reset, and restart behavior is explicit; readers never observe a partial update.

Expected touch points: metric accumulator and focused event-trace tests.
Smallest check: the focused exact-trace test.
Prerequisites: operational metric schema and engine event stream.
Owned result: in-memory metric updates and snapshot generation only.
Claim: unassigned.
