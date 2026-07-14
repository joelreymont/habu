---
title: Add immutable nominal layout arena
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T15:25:39.586847+02:00"
blocks:
  - habu-protect-dynamic-immutable-eccd0489
  - habu-nominal-storage-effect-a60ba885
  - habu-seal-owners-syntax-63051652
---

Full context: static CAD effect rows and other large canonical typed sets cannot be wide by-value PRODUCTs: ER.MINI limits binary input shape to 255 cells, and even the maximal 127-cell row holds only 25 five-cell bindings, while repeated call-site REMAP/UNION in real models reaches hundreds or thousands. Fix: add a sealed immutable layout arena whose public values are one-cell opaque nominal handles and whose private index-based path, binding, chunk, and row records are append-only behind a linear transactional builder. Persistent paths are consed stable lexical segments with no small fixed depth; rows are sorted unique binding sets; UNION streams sorted chunks, REMAP preserves exact paths/slots, freeze validates and full-content-interns, and published records never mutate. Handle numbers, offsets, pointers, allocation order, and internal hashes are never semantic identity: equality, diagnostics, cache keys, replay, AOT, and wire formats use canonical contents. Rollback restores high-water/index state before escape; snapshot/AOT validate bounds, sortedness, uniqueness, full consumption and high-waters; fixpoint bytes are allocation-order independent. Acceptance: no public raw cast or arena pointer; forged/stale/cross-owner handle, partial builder, mutation after freeze, index/wire corruption, allocator failure, and overflow return typed failures without publication; canonical insertion-order and fresh-process bytes match; compose/remap/UNION/snapshot/replay/AOT at 4096 distinct bindings; full-row duplicate is idempotent; builder/merge growth is measured non-quadratic; resource/policy budgets are separate from semantic capacity; protected spans prevent every raw mutation. Files: one concern each for path, binding/chunk, row arena, builder, canonical codec, snapshot/AOT adapters and focused/property/scale tests; no CAD atom policy. Verify: nominal forgery matrix, rollback/snapshot/AOT/replay/fixpoint parity, performance bound, trust/refine/typed-local lints, full gate. Ownership: reusable immutable nominal collection substrate; CAD-EFFECT consumes it but defines atoms and legality separately.
