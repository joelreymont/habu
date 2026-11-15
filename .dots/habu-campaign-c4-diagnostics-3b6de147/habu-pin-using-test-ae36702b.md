---
title: "Pin using-test's remaining records whole"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:30:07.320449+01:00"
closed-at: "2026-11-16T04:07:35.130120+01:00"
---

Problem: test/using-test.f ~:85-87 (VS-CATCH), ~:229 and ~:397 check only a code, so a C4 defect in those records would pass. Acceptance: every record test/using-test.f pins is checked by its first line, label and help. Each checks the record's first line, label and help. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
