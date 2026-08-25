---
title: "Maki: migrate tensor enums"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:17:20.314566+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own maki/tensor.f and maki/tensor-value.f declarations/constructor consumers plus focused tests. Convert payloadless declarations to compact ENUM, preserving tag ordinals, package spellings, serialized identities, derives, and tensor execution semantics.
