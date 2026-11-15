---
title: Reject an over-long counted string in --verify-only
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:55:00.963962+01:00"
closed-at: "2026-11-16T04:07:34.585297+01:00"
---

Problem: --verify-only accepts c" with 300 bytes that the run and check.f reject. Acceptance: bin/hb FILE, check.f and --verify-only print one and the same record for it, rc 70 (74 for an unreadable file). Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
