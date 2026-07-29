---
title: Add KV layer dimension
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T21:16:42.471542+02:00"
blocks:
  - habu-parse-gpt-2-c8baa4db
  - habu-use-opaque-kv-46d39625
---

Problem: KV:kvcfg and page bytes describe only 2 * kv-heads * head-dim * dtype bytes per token; a transformer has one K/V pair per layer, so the current pool cannot address GPT-2 without aliasing every layer. Result: hard-cut KV:kvcfg and CONFIG/CONFIG-P to require positive layer-count, compute page bytes = layers * page-tokens * 2 * kv-heads * head-dim * dtype-bytes with checked arithmetic, and lay each physical page out as layer-major token rows. Keep one page identifier and one block table. Private address calculation takes an authenticated layer index and sequence position; no raw pointer becomes public. Delete the old arity and layout. Add no version, migration reader, per-layer allocator, compatibility config, or second block table. Owner: KV configuration, pool geometry, and private address arithmetic only. Dependency: the parsed GPT-2 config supplies the first real geometry. Production red: GPT-2 layer 1 aliases layer 0 because no layer coordinate exists. Acceptance: exact first/last layer-token-head byte offsets match the formula; one-over in every product rejects before allocation; old CONFIG arity does not resolve; allocator reservation, fork, and cancellation remain page-based; focused KV, GPT-2 geometry, package, and exact-diff gates pass. Claim: unassigned.
