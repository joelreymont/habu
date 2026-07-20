---
title: Split HB build maker and cache
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-15T23:50:57.703346+02:00\""
blocks:
  - habu-split-hb-build-c3bdd7eb
---

Full context: after compose and lints extraction, move maker key/source/build lifecycle into tools/hb-build-maker.f and artifact/object key, lock, restore/store/install lifecycle into tools/hb-build-cache.f under reopened HB-BUILD. Rename private collisions RESTORE-OBJECT? and BUILD-ELAPSED; keep cache keys byte-identical and add every component to the maker closure key. No public cache internals. Acceptance: cold, maker, artifact and object hit/miss/invalidation fixtures pass; lock/error paths propagate; no legacy names resolve.

Claim: agent=hbsplit workspace=.jj-ws/fable-hbsplit machine=spark (owns tools/hb-build-maker.f + tools/hb-build-cache.f split + fixtures)
