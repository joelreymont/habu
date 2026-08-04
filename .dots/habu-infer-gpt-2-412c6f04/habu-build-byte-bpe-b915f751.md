---
title: Build byte-BPE tables
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:48.806989+02:00"
blocks:
  - habu-own-gpt-2-45d7d1e4
---

Why: byte-BPE vocabulary and merge construction must be atomic and independent from encoding. `BPE:OPEN` already returns an initialized `BPE:builder`; there is no public BEGIN or runtime ready flag. `ADD-VOCAB` and `ADD-MERGE` consume one builder and return generic `result<BPE:builder,n>`; malformed pinned input is terminal, so an error arm releases the consumed builder internally instead of inventing a recoverable owner payload. `SEAL ( BPE:builder -- result<BPE:state,n> )` validates completeness, uniqueness, byte-map consistency, and merge order before retyping the same allocation; its error arm likewise releases the builder. `CLOSE` remains the explicit caller-chosen abandonment path. Owner: BPE table population and builder-to-state transition only. Production red: landed tables are singleton globals and readiness is a runtime flag. Acceptance: two builders interleave; exact merge order, duplicate vocabulary or merge entries, short capacity, malformed byte maps, incomplete sealing, and injected terminal failures leave no owner or mapping leak; encode/decode reject `BPE:builder` statically and accept only `BPE:state`. Forbidden: tokenizer assets, encode/decode algorithm, global table, callback, fallback vocabulary, version, compatibility alias, catch across a live owner, special result type, or repair-in-place error arm. Smallest owning check: bin/hb --load maki/infer/bpe-table-test.f.

Capacity-order acceptance: every `BPE:OPEN` call uses the frozen named capacity constants in order, and a mutation swapping distinct vocabulary and merge capacities fails through this real table-builder entry. No state getter or allocator-observation seam exists solely for this check. `bpe-table.f` reopens BPE but does not seal it; the assembling `bpe.f` seals the private and public wordlists only after table, encode, and decode definitions load, and the combined test proves package reopen plus direct private/public WID publication all fail with exit 84.
