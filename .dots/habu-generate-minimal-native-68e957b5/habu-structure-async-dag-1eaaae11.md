---
title: Structure async DAG state
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:22:48.991023+02:00"
---

maki/async-dag.f:105-121 stores stream tails and event lifecycle as raw parallel arrays: AS-TAIL and AEV-REC use node+1 with zero as none, while AEV-DEAD is an independent boolean. Edges at :190-196 are parallel raw from/to indexes, and replay order later reuses raw indexes plus -1 scratch markers. The checker accepts node/stream/event/count swaps and represents inconsistent event states such as destroyed plus an arbitrary record. The existing habu-maki-migrate-async-70c64d90 owns the main node STRUCTURE and akind ENUM, but not these auxiliary tables. After that representation owner lands, store stream tails as option<ADAG:node-id>, define a payload ENUM event-state (live-unrecorded | live-recorded(node-id) | destroyed) in LAYOUT-BUFFER, and store edges as a STRUCTURE with typed from/to node-id fields. Use typed replay-order storage and a separate emitted marker/ENUM rather than overloading node values with -1. Eliminate +1/-1 codecs outside any explicit persistence boundary and match event state exhaustively. Preserve program-order edges, latest-record waits, destroy semantics, topological order, hashes, replay, and fixed capacities. Add checker negatives for every ID/edge-field/state swap; lifecycle transition tests cover record/rerecord/wait/destroy/double-destroy/unready/sealed paths; edge canaries and graph goldens prove no partial mutation. Measure JIT/DATA/CODELEN, layout bytes, and seal/replay throughput before/after. Files: maki/async-dag.f and focused tests. Verify async DAG/plan/replay/snapshot, Maki, typed-local diff, type/package/host/filemap/dot lints, and full native gate. Ownership: auxiliary stream/event/edge/replay state only.
