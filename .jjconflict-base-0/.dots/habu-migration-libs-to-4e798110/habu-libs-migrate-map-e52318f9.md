---
title: "Libraries: migrate MAP types"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:17:01.730811+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own lib/map.f declarations/constructor consumers and focused map tests. Convert payloadless mode enum to compact ENUM and MAP--LOC positional SUMTYPE to full named-field ENUM, preserving derived package names, tag ordinals, generic schemas, layout, MATCH behavior, and public effects.
