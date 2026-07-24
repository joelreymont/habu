---
title: Replace publication whiteboxes
status: open
priority: 2
issue-type: task
created-at: "2026-07-24T20:49:38.698256+02:00"
blocks:
  - habu-replace-snapshot-failure-f138b2e9
---

Why: sections 11 and 13 of test/decl-event-suite.f call private field insertion and DEV-PREPARE, DEV-COMMIT, and DEV-FINALIZE. Owner: one focused checked-Habu production mutation test, test inventories, FILEMAP.md, and the public replacement in test/decl-event-suite.f. Exact result: suppress only the event-row append after public DECL-EVENT:FIELD adds its field row; public PUBLISH must reject 7162 before public event or field publication and leave the token rollbackable. Before END-VARIANT, public payload readers reject 7172; after normal public PUBLISH, the consumed token rejects 7161 and committed reflection stays exact. Remove the committed-but-not-finalized observation because generic phase ordering is owned by test/generated-declaration-transaction-suite.f. Delete TEST-ADD-UNTRACKED-FIELD, direct field-token reads, private phase calls, and DEV-FLD-TX-CELLS-FOR calls. Forbidden: public lifecycle additions, test participant, trusted bridge, raw field token, copied preflight, private phase call, or weaker final-state checks. Smallest checks: focused mutation test and declaration-event suite. Depends: Replace snapshot failure whitebox. Claim: unassigned.
