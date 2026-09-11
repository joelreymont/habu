---
title: Freeze transient MEM spans for persistence
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:44:58.346692+02:00"
blocks:
  - habu-invalidate-mem-evidence-1c2dfdb4
---

Problem: transient process pointers and borrows can currently be serialized or cached as though their addresses and lifetime survive replay. Fix: add explicit package MEM FREEZE/owned-copy transition that consumes a transient span and produces immutable persistent bytes with canonical content identity; no raw pointer, region counter, allocation order, or generation becomes persistent authority. Acceptance: serializing transient/borrowed/unique state rejects; frozen content round-trips, changes digest on every byte mutation, survives source owner release, and rejects stale schema/content mismatch. Files: lib/memory-region-freeze.f, focused test, canonical bytes owner, docs/effects.md. Verify: checker negatives, deterministic round-trip/mutation matrix, memory/object/store slices, typed-local diff lint, full native gate.
