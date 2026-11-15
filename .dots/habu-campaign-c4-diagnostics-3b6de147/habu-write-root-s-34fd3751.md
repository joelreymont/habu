---
title: "Write ROOT!'s record for a path with a NUL"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:01:40.353668+01:00"
closed-at: "2026-11-16T04:07:33.639139+01:00"
---

Problem: ROOT! with a string holding a NUL goes ROOT-SET (src/core/include.f) → JOIN → ROOT-N → CANON-N → PATHZ and throws E-PATH-NUL bare, with no record; ROOT-SET has no LOADER-PATH-FAULT check. Acceptance: one E-PATH-NUL record, rc 70. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
