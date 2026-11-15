---
title: Reject an unreadable named input before the lints
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T22:31:41.853262+01:00"
closed-at: "2026-11-16T04:07:34.552373+01:00"
---

Problem: check.f --all-errors on a named file that will not open exits 67 with E-THROW: the walk's record is dropped and the reserved-name lint reads the file (E-FS-OPEN). Acceptance: the check gives the one E-UNREADABLE-SOURCE record, rc 74, in every mode. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
