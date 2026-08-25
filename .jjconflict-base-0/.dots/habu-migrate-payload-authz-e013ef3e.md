---
title: Migrate payload authorization tests
status: open
priority: 2
issue-type: task
created-at: "2026-07-24T20:49:38.703942+02:00"
blocks:
  - habu-replace-publication-whiteboxes-f6704aec
---

Why: sections 12, 15, 16, and 17 of test/decl-event-suite.f define tests inside package DECL-EVENT and inspect or corrupt private rows and frames. Owner: test/decl-event-suite.f only. Exact result: recreate their observable contracts in package DECL-EVENT-PUBLIC-TEST using qualified public operations only. Acceptance: nested different-family declarations retain independent payload authorization; invalid variant names reject before a later valid variant and rollback; every family-scoped mutator rejects foreign and sentinel families while leaving the original token usable; two public payload fields return exact declaration order, schemas, widths, and total cells; wrong token, family, variant, negative index, and one-past index reject with existing codes; rollback and publication retain exact public counts, identity, family, variant, and field reflection. Delete all DEV-N, DEV-PUB-N, DEV-ROW, DEV-PROV, DEVTX, DEV-TX-TOP, owner, ordinal, selector, raw-byte swap, and provisional-count assertions. Forbidden: package DECL-EVENT reopen, private identifier, trusted word, raw mutation, copied event model, or substring proof. Smallest check: bin/hb < test/decl-event-suite.f. Depends: Replace publication whiteboxes. Claim: unassigned.
