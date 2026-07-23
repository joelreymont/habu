---
title: "ENUM: finalize family kind"
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T14:53:49.128448+02:00"
blocks:
  - habu-enum-expose-provisional-bcb7c765
---

Why: full ENUM currently fixes TK-SUM before it has parsed variants, so a full declaration with no fields cannot become the canonical tag-only TK-ENUM representation. A generic raw kind mutator would violate immutable publication.

Owner and interface: add sealed pre-hook package TYPE-FAMILY-OWNER with only FINALIZE-ENUM-KIND ( family has-payload -- kind ). It accepts only a live TK-SUM or TK-ENUM family, stores TK-SUM when has-payload is true and TK-ENUM otherwise, and returns the stored kind. DECL-EVENT adds one inventoried bridge DEV-FAM-FINALIZE-ENUM-KIND and public FINALIZE-ENUM ( declaration-token family -- kind ). The checked word requires the exact live top declaration token and bound family, requires no open variant, proves at least one variant event, scans only that frame events, treats the presence of any field event as payload regardless of cell width, then calls the owner. No caller supplies the classification bit.

Immutability: generated-declaration protection undefines TYPE-FAMILY-OWNER:FINALIZE-ENUM-KIND after DECL-EVENT compiles. The compiled DECL-EVENT capability remains callable only with a live token. No public raw kind setter, post-publication mutation, mode heuristic, positional payload query, legacy edit, constructor call, or front-end change.

Acceptance: raw DECL-EVENT transactions prove payloadless compact-shaped and full-shaped event streams finalize to TK-ENUM, any field in any variant finalizes to TK-SUM, and wrong token/family/open-variant/empty-declaration/stale/post-publish/post-rollback calls reject before kind mutation. Nested transactions cannot finalize the outer family. Rollback after finalization removes the provisional family and leaves prior family bytes unchanged. Snapshot persistence preserves the chosen kind. Mutations of token validation, frame bounds, field-event classification, either chosen kind, or capability retirement fail. Files: src/core/type-family.f, src/core/decl-event.f, generated-declaration protection, focused suites, TRUSTED.md, and inventories only. Smallest check: bin/hb --load test/decl-event-suite.f. Run type-family, decl-event, generated-declaration transaction, snapshot, exact diff lints, strict trust/inventory, candidate validation, and native fixpoint.
