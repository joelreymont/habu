---
title: "Infer KV: snapshot publication handshake"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:38:16.962731+02:00"
blocks:
  - habu-infer-kv-immutable-1ec13a88
---

Why this exists:
immutable snapshot bytes alone do not define when a GPU launch may consume a new generation or when the host may reclaim the old generation.

Required result:
add an explicit versioned publish/acquire/retire handshake with one publisher and generation-checked device leases.

Done when:
publish is all-or-nothing, readers see one complete generation, old storage is retained until the final lease retires, stale/double retire rejects, and cancellation cannot reclaim a snapshot in flight.

Expected touch points: new maki/infer/kv-snapshot-sync.f, new maki/infer/kv-snapshot-sync-test.f, maki/maki.f, FILEMAP.md.
Smallest check: bin/hb --load maki/infer/kv-snapshot-sync-test.f; filemap lint.
Prerequisites: immutable device snapshot.
Owned result: snapshot synchronization and lease state only.
Claim: unassigned.
