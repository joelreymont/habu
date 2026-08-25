---
title: "Maki: migrate report plan enums"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:17:36.930462+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own maki/report.f and maki/fusion-plan.f declarations/consumers plus focused tests. Convert payloadless declarations to compact ENUM, preserving tag ordinals, names, derives, JSON/report identities, plan semantics, and public effects.
