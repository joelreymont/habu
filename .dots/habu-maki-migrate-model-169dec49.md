---
title: "Maki: migrate model IR types"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:17:42.869374+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own maki/model-ir.f record/enum declarations and focused model-IR tests. Convert PRODUCT/raw records to typed STRUCTURE and payloadless variants to ENUM, preserving field/tag order, package APIs, serialized graph identity, layout, derives, and model passes.
