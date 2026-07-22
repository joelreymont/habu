---
title: "Fields: record package visibility"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T12:23:25.405306+02:00"
blocks:
  - habu-exhaust-field-reserved-9f0b6bcf
---

Problem: the shared PF row records family, variant, name, schema, layout, and flags but cannot decide whether a generated field operation belongs to the public surface or only the declaring package. Acceptance: extend each field row with canonical defining-package identity and a closed public/private visibility value captured from the declaration owner before row publication. Validate family/package ownership transactionally, include package and visibility in semantic field identity, and expose committed read-only TYPE-FIELD:PACKAGE@ and TYPE-FIELD:VISIBILITY@ reflection without a raw row pointer. Rollback, snapshot, ahead-of-time, replay, and fixpoint preserve the values. This leaf does not add source provenance or provisional readers. Files: shared PF record, declaration producer, typed reflection, and focused package/field tests. Verify: public/private fields, package reopen, qualification, cross-package collision, malformed visibility, rollback, snapshot/AOT/replay, and native/recovery parity. Depends: habu-exhaust-field-reserved-9f0b6bcf. Ownership: committed field package and visibility metadata only. Claim: unassigned.
