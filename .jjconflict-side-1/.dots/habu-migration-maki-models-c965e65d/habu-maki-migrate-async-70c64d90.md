---
title: "Maki: migrate async DAG types"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T18:10:06.197028+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own maki/async-dag.f TYPEFAMILY stream-id event-id and node-id, PRODUCT node, legacy payloadless ENUM akind, all consumers, and async-dag tests. Convert nominal cell families and node record to STRUCTURE and akind to compact ENUM with no arity or headers. Preserve tag ordinals, field order, package spellings, DAG hashes, deterministic replay, schedule legality, serialized identity, and public effects. Run async-dag, plan-ir, maki, ptx-stdlib, snapshot, and replay gates.
