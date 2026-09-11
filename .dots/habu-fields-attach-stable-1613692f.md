---
title: "Fields: attach stable source origins"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T12:23:25.604808+02:00"
blocks:
  - habu-fields-expose-provisional-96533716
  - habu-src-origins-re-5ebf336d
---

Problem: field diagnostics still depend on transient parser coordinates and PF rows have no durable declaration provenance. Acceptance: extend each PF row with one validated immutable source-origin id captured from the live declaration before publication. Add committed TYPE-FIELD:ORIGIN@ and token-gated DECL-EVENT:FIELD-ORIGIN@ reflection; both expose the origin handle, never a parser pointer or raw PF row. Serialize and re-intern origins for snapshot, ahead-of-time, replay, recovery, and fixpoint. Provenance is excluded from family, field, constructor, layout, and artifact semantic hashes, so moving identical source changes diagnostics only. Files: PF origin field, declaration producer, origin reflection, diagnostics, and focused persistence tests. Verify: nested include/evaluate spans, rollback without leaked rows, stale origin, moved source, snapshot/AOT/replay, and exact JSON/text field diagnostics. Depends: Fields: expose provisional descriptors and habu-stable-source-origin-frame-9d4b2a61. Ownership: attaching and reflecting source origins on field rows only. Claim: unassigned.
