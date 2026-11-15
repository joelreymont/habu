---
title: Write a placed record for a bad FFI declaration
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:55:00.989547+01:00"
closed-at: "2026-11-16T04:07:33.799189+01:00"
---

Problem: a bad FUNCTION: or CALLBACK: declaration ends with an uncaught -3501. Acceptance: bin/hb FILE, check.f and --verify-only print one and the same record for it, rc 70 (74 for an unreadable file). Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
