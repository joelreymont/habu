---
title: "ENUM: expose named reflection"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:14:22.699583+02:00"
blocks:
  - habu-enum-infer-canonical-f07a77c2
---

Own ENUM read-only reflection integration and focused tests. Expose variant names/tags and each named field schema/offset without mutable registry pointers; prove compact and payload forms round-trip identically through rollback, snapshot, hashing, and public signature enumeration.
