---
title: Reject a source that fails mid-read
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:23:15.639302+01:00"
closed-at: "2026-11-16T04:07:34.498677+01:00"
---

Problem: a source that opens but fails to read ends the run with 'include: read failed' prose; the verifier throws E-SOURCE-READ with no record. Acceptance: one record, rc 74, in all three readers. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
