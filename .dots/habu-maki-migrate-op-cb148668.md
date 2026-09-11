---
title: "Maki: migrate operation enums"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:17:27.734740+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own maki/op-kind.f declaration/consumers and focused tests. Convert to compact ENUM while preserving op tag ordinals, package spelling, dispatch behavior, serialized identities, derives, and public signatures.
