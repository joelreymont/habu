---
title: Write the ;using records in REJECTION
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:59:01.233661+01:00"
closed-at: "2026-11-16T04:07:33.605937+01:00"
---

Problem: E-USING-UNBALANCED and E-USING-OUTER each have two writers, src/core/checker.f and src/habu/interpreter.f. Acceptance: one writer each in src/core/rejection.f, called by both; the three readers print the same record. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
