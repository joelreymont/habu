---
title: Join a quotation read from storage with one that reads a local
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T11:31:36.475546+02:00"
---

Problem: a quotation fetched from a TYPED-VARIABLE or through a DERIVE addr field accessor carries the storage cell block variable, which a store has marked reads-no-local, so joining it at then with a quotation that reads a local is refused with a quotation that reads a local would outlive it, though nothing is stored or returned. Reproducers ~/.cache/tmp/quot-rereview/q/j01-var-join.f and j04-addr-join.f, rc 70 at then on the quotation tip engine; the same join through UNMAKE or a MATCH payload is accepted (j02, j03). Found by the fix re-review of the quotation line; present before fix B. Acceptance: both programs check and run (j01 prints 4 1); a store of a reading quotation through the same pointer is still refused. Files: src/core/checker.f (the fetch row for a pointer to a quotation). Verify: COK-QL cases in test/engine-suite.f for both joins plus the existing CBAD-QL-STORED. Depends: none. Ownership: unassigned. Claim: unassigned.
