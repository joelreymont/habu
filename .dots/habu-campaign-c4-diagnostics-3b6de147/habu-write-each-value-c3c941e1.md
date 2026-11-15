---
title: Write each value-record and DEFLINEAR rejection once
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:55:00.882818+01:00"
closed-at: "2026-11-16T04:07:33.764125+01:00"
---

Problem: the loader and the verifier write different records for a bad VALUE-RECORD or DEFLINEAR; the loader's field record is unplaced. Acceptance: bin/hb FILE, check.f and --verify-only print one and the same record for it, rc 70 (74 for an unreadable file). Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
