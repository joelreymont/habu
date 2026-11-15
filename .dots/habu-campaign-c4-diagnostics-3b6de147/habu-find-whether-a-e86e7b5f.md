---
title: Find whether a deferred verdict reaches DEFINITION
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:01:40.394212+01:00"
closed-at: "2026-11-16T04:07:36.212217+01:00"
---

Problem: REPORT-DEFERRED (src/core/check-hook.f) writes a deferred verdict's record, but no entry found brings VERDICT-DEFERRED to DEFINITION: the marks are written by verify-source scans and WITH-TICK-ORDER is called only from verify-source.f. Acceptance: a reproducer that reaches it, or REPORT-DEFERRED deleted. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
