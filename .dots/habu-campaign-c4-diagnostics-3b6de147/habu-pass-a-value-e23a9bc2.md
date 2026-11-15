---
title: "Pass a value record's field list in place"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:23:15.628455+01:00"
closed-at: "2026-11-16T04:07:35.205098+01:00"
---

Problem: src/core/roles.f VALUE-RECORD copies its fields into a 4096-byte buffer and rejects a longer list (E-VALUE-RECORD-TOO-LONG) though the input holds the list whole. Acceptance: a field list of any length is accepted; the buffer and the code are gone. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
