---
title: Write one record for an unclosed declaration
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:55:00.892611+01:00"
closed-at: "2026-11-16T04:07:33.687696+01:00"
---

Problem: an unclosed VALUE-RECORD gets two texts; an unclosed BEGIN-STRUCTURE stops the verifier with no record; a long field list dies raw. Acceptance: bin/hb FILE, check.f and --verify-only print one and the same record for it, rc 70 (74 for an unreadable file). Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
