---
title: "Field projection: authorize live rows"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T12:23:25.543037+02:00"
blocks:
  - habu-fields-expose-provisional-96533716
  - habu-checker-type-structure-d996215b
---

Problem: FIELD-PROJ currently arms only from committed TYPE-FIELD rows, while atomic STRUCTURE generation certifies accessors before field commit. Premature row publication or a broad trusted read would break declaration atomicity. Acceptance: make the sealed FIELD-PROJ arming path carry the exact live declaration token, accessor name, family id, field id, and byte offset. It validates the token-authorized provisional descriptor, family application, addressability, package visibility, schema, byte extent, and exact offset, then arms one single-shot field-project use for that accessor. Reject any mismatch before checker state changes and disarm on success or failure. User code cannot tick or execute the arming surface. Files: field-projection checker owner, DECL-EVENT bridge, and focused projection tests. Verify: public/private, generic, nested, byte, pointer, wrong token/family/field/name/offset/visibility/schema, double-use, reject, rollback, snapshot, and AOT fixtures. Depends: Fields: expose provisional descriptors and habu-checker-type-structure-d996215b. Ownership: provisional authorization of the existing field-project capability only. Claim: unassigned.
