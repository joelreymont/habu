---
title: "Migration: core variants to ENUM"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:46:13.909391+02:00"
blocks:
  - habu-compiler-lower-unified-5f599080
---

Migrate src/core and src/habu SUMTYPE declarations and payloadless raw-tag families to unified ENUM variants with named inline fields. Preserve constructor package spellings where the family name is unchanged, MATCH semantics, tag ordinals, layout policy, derived operations, serialized identities, and checker diagnostics. Remove positional payload syntax from core sources. Run type-family, declaration, lowering, engine, snapshot, AOT, and fixpoint gates.
