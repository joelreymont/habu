---
title: "Infer KV: atomic prefix fork"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:38:16.936337+02:00"
blocks:
  - habu-infer-kv-fixed-a219f7ba
  - habu-infer-kv-declared-a0319bef
---

Why this exists:
prefix fork increments multiple page references and publishes a child slot; overflow or capacity failure partway through can leak references or expose a partial child.

Required result:
preflight child capacity, every reference increment, tail copy-on-write reserve, and generation advance before an infallible commit.

Done when:
failure at each preflight leaves parent, refs, free list, reservations, and all dead slots unchanged; success shares every complete page and reserves or copies only the mutable tail.

Expected touch points: maki/infer/kv-cache.f, maki/infer/kv-cache-test.f.
Smallest check: bin/hb --load maki/infer/kv-cache-test.f.
Prerequisites: fixed block-table geometry and declared maximum admission.
Owned result: prefix fork transition only.
Claim: unassigned.
