---
title: "Mark tick order unknown at a trusted body's immediate"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T20:29:02.262945+01:00"
---

Hypothesis, from reading src/habu/verify-source.f SCAN-TRUSTED-BODY: a colon body marks tick order unknown at an immediate, whose run may define words, and a TRUSTED: body's scan does not, so a tick after it may be judged against words the immediate changes. Acceptance: a program that shows it, or the reason none can, and where one does, load and check answer alike with a case. Depends: none. Claim: unassigned.
