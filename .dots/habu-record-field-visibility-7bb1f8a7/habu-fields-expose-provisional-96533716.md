---
title: "Fields: expose provisional descriptors"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T12:23:25.485798+02:00"
blocks:
  - habu-fields-record-pkg-e42a9223
  - habu-atomic-generated-declaration-4c1e8b7a
---

Problem: generated operations are planned and checked before PF rows commit, but current TYPE-FIELD reflection rejects every provisional id. Acceptance: add DECL-EVENT token-gated provisional reflection for FIELD-NAME$, FIELD-SCHEMA@, FIELD-BYTE-OFF@, FIELD-BYTES@, FIELD-FLAGS@, FIELD-PACKAGE@, and FIELD-VISIBILITY@. Every call receives the exact live declaration token, family id, and field id; it verifies the active transaction, owning family, provisional range, and requested field before returning a copied span or scalar. No raw row pointer, ambient current-family fallback, or committed-row bypass is exposed. Commit invalidates the provisional token and existing committed reflection remains unchanged. Files: DECL-EVENT/PF transaction bridge and focused authorization tests. Verify: correct live reads plus stale token, wrong family, wrong field, prior transaction, committed token, nested transaction, rollback, and row-forgery negatives. Depends: Fields: record package visibility and habu-atomic-generated-declaration-4c1e8b7a. Ownership: authorized provisional field reflection only. Claim: unassigned.
