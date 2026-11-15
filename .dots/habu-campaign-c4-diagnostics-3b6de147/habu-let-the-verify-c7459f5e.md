---
title: Let the --verify-only child write its own records
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:55:00.980813+01:00"
closed-at: "2026-11-16T04:07:35.589014+01:00"
---

Problem: the child reports a stop as protocol lines the parent turns into an old packet. Acceptance: bin/hb FILE, check.f and --verify-only print one and the same record for it, rc 70 (74 for an unreadable file). Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
