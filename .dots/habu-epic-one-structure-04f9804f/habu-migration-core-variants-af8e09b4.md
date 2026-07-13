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

Census correction: current master has no live src/core or src/habu SUMTYPE
declaration. Treat this as a bounded destruction pass after unified lowering:
prove the core declaration census is empty, migrate any raw-tag family the
token-aware census identifies, and otherwise close with the zero-finding
artifact. Parser, scanner, and rejection cleanup belongs to the delete/tool
dots and must not be duplicated here.
