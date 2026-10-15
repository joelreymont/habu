---
title: Reclaim browser response storage
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T06:43:03.708852+03:00"
---

Read-only Maki resource review at f80e9331 against public b685c051-xml-zip-ascii-1f0b72ec found src/lib/browser/turn.js appends page-rounded host storage for every nonempty response, typed input and key event, with no release/retain protocol. A one-byte response/key consumes another 64 KiB. Maki replaces/clears its current scene and discards stale replies, but cannot return any historical response allocation. Add the smallest public ownership/reclamation boundary for host-delivered buffers that permits one retained scene and temporary responses; preserve valid lifetimes and refuse stale use. Coordinate caller migration with Maki owner, with no application-specific camera or JS adapter. First inspect existing browser storage E2E, then demonstrate repeated accepted and obsolete responses do not grow live storage without bound while the retained scene remains readable. Keep this after current incremental-build work; no speculative allocator framework or linear-local dependency.
