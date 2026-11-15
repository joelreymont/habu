---
title: "Give the verifier the loader's missing-name records"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:55:00.934363+01:00"
closed-at: "2026-11-16T04:07:35.991010+01:00"
---

Problem: --verify-only writes a generic missing-name record where the loader writes each definer's own. Acceptance: bin/hb FILE, check.f and --verify-only print one and the same record for it, rc 70 (74 for an unreadable file). Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
