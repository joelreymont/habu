---
title: "Infer KV: exact physical page metrics"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T09:38:16.952672+02:00"
blocks:
  - habu-infer-kv-fixed-a219f7ba
  - habu-infer-kv-declared-a0319bef
---

Why this exists:
the allocator lacks a single exact physical-accounting surface for total, free, live, reserved, shared, high-water, tail-waste, page bytes, and bytes per token.

Required result:
derive each metric from physical allocator state with checked geometry and define whether shared pages count once.

Done when:
metrics match a recomputed oracle through allocation, append, fork, cancellation, exhaustion, and disposal; total equals free plus live; high-water is monotonic; tail waste never double-counts shared pages.

Expected touch points: maki/infer/kv-cache.f, maki/infer/kv-cache-test.f.
Smallest check: bin/hb --load maki/infer/kv-cache-test.f.
Prerequisites: fixed block-table geometry and declared maximum admission.
Owned result: metric queries and metric oracle tests only.
Claim: unassigned.
