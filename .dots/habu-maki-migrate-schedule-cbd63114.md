---
title: "Maki: migrate schedule key types"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:17:49.937165+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own maki/sched-key.f record/enum declarations and focused key/cache tests. Convert PRODUCT records to typed STRUCTURE and payloadless variants to ENUM, preserving hash/equality derivation, field/tag order, package spellings, cache keys, and wire identity.
