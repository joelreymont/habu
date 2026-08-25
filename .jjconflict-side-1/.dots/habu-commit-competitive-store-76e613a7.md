---
title: Commit competitive store records atomically
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:03:14.944087+02:00"
---

maki/competitive-store.f publishes one semantic record through multiple durable writes, so interruption can expose a torn relation even though each row is syntactically valid. Define one immutable semantic record and one transaction boundary for all related rows. Use a framed commit record or content-addressed generation plus atomic head publication so recovery sees either the complete old generation or complete new generation, never a mixed pair. Validate and stage every field before writing, sync data and directory through the existing durability owner, publish once, and make replay reject incomplete, duplicate, conflicting, or orphaned members with exact diagnostics. Concurrent writers must serialize or deterministically resolve by record identity without lost updates. Add crash injection after every byte/write/sync/rename/head step, old/new recovery proof, duplicate and conflict cases, concurrent writers, and byte-stable valid replay. Coordinate shared framing with habu-factor-maki-store-24dc8f8b and generic replay atomicity with habu-make-store-replay-7cd1f6d7, but own only competitive semantic multi-record commit. Files: competitive store and focused tests. Verify competitive evidence/report/store suites, typed-local/package/host/dot lints, and full native gate.
