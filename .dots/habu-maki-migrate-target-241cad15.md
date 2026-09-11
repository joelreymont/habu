---
title: "Maki: migrate target descriptor"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:18:01.810631+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own maki/target/target.f SUMTYPE declaration/constructor consumers and focused target tests. Convert to full ENUM with named payload fields, preserving TARGET-DESCRIPTOR:* package spelling, tag ordinals, target/dialect dispatch, serialized identity, and public effects.
