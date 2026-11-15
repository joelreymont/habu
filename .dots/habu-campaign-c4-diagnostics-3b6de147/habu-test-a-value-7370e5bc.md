---
title: "Test a value record field's bad type"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:01:40.385697+01:00"
closed-at: "2026-11-16T04:07:33.889975+01:00"
---

Problem: no test makes a value record whose field has no type or an unknown or malformed type, so VREC-TYPE-REJECT (src/core/checker.f) never runs under a suite. Acceptance: a suite pins both records whole. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
