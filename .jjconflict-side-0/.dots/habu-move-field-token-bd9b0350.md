---
title: Move field-token authority tests
status: open
priority: 2
issue-type: task
created-at: "2026-07-24T20:49:38.678226+02:00"
---

Why: section 18 of test/decl-event-suite.f obtains the private declaration frame field token to test TYPE-FIELD-OWNER:TX-CELLS-FOR; that proof belongs to the field owner. Owner: test/type-family-suite.f and removal from test/decl-event-suite.f. Exact result: move the full positive and negative matrix to the existing type-family test package. Use its existing TWX-PF-BEGIN, TWX-PF-ADD, and TWX-PF-ROLLBACK seams to create the transaction and rows, then call the public TYPE-FIELD-OWNER:TX-CELLS-FOR. Acceptance: exact live token/family/row returns the stored cell count; wrong token rejects E-PF-TX; wrong family and another valid family row reject E-PF-OWNER; negative and one-past identifiers reject E-PF-ID; every rejection preserves the live transaction and the positive query still succeeds. Delete section 18, TXC helpers, and private DEV-TX-TOP and DEVTX.FLDTOK access from the declaration-event suite. Forbidden: production edits, new trusted words, declaration-frame access, copied field owner, raw row pointers, aliases, or reduced negative coverage. Smallest checks: bin/hb --load test/type-family-suite.f and bin/hb < test/decl-event-suite.f. Depends: none. Claim: unassigned.
