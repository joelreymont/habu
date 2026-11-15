---
title: Spell a clause clash alike in load and check
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:52:14.026983+01:00"
closed-at: "2026-11-16T04:07:34.026138+01:00"
---

Problem: for an export written in another case than its definer (EXPORT drp:drd), the load names the clause as the definer's record spells it, while src/core/checker.f EXPORT-ROOM spells it as the export wrote it. Acceptance: a reproducer shows both readers print the same record. Depends: habu-write-one-record-33a0a591. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
