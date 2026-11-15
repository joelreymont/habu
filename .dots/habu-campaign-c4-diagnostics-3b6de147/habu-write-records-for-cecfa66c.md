---
title: "Write records for the arm64 linker's rejections"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:55:00.972727+01:00"
closed-at: "2026-11-16T04:07:33.652316+01:00"
---

Problem: src/habu/link-data.f dies with 76 and a raw line. Acceptance: bin/hb FILE, check.f and --verify-only print one and the same record for it, rc 70 (74 for an unreadable file). Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
